{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Atomic where

import qualified PlutusLedgerAPi.V2       as PlutusV2
import           PlutusLedgerAPI.V1.Value (geq)
import           PlutusTx                 (CompiledCode, compile, unstableMakeIsData)
import           Utilities                (IO)


data AtomicDatum = AtomicDatum
    { adAddr :: Address
    , adPrice :: Value
    }

PlutusTx.unstableMakeIsData ''AtomicDatum -- create an instance for IsData

mkAtomicValidator :: AtomicDatum -> () -> PlutusV2.ScriptContext -> Bool
mkAtomicValidator AtomicDatum {..} () ScriptContext {..} = 
    any _ $ txInfoOutputs scriptContextTxInfo 
  where
    p :: TxOut -> Bool
    p TxOut {..} = txOutAddress == adAddr && txOutValue `geq` adPrice
{-# INLINABLE mkAtomicValidator #-}

atomicValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
atomicValidator = $$(PlutusTx.compile [|| wrappedAtomicValidator ||])
    where
        wrappedAtomicValidator = wrapValidator mkAtomicValidator



---------OFF-CHAIN-------

scanAtomic :: GYTxQueryMonad m => m (Map GYTxOutRef (GYValue, AtomicDatum))
scanAtomic = do
    contractAddr <- atomicAddress
    utxos        <- utxosAtAddress contractAddr
    witherUTxOs atomicDatum utxos

atomicDatum :: GYUTxO -> Maybe (GYValue, AtomicDatum)
atomicDatum utxo = do
    addr <- atomicAddress
    e    <- utxoDatum utxo
    pure $ case e of
        Left _          -> Nothing
        Right (a, v, d) 
            | a == addr -> Just (v, d)
            | otherwise -> Nothing


buyAtomic :: GYTxMonad m => GYTxOutRef -> m (GYTxSkeleton 'PlutusV2)
buyAtomic ref = do
    utxo <- utxoAtTxOutRef' ref
    m <- atomicDatum ref
    case m of
        Nothing           > error "invalid UTxO ref"
        Just (toSell, d) -> 
            price <- valueFromPlutus' $ adPrice d
            price <- addressFromPlutus' $ d
            let datum = datumFromPlutusData d
                redeemer = redeemerFromPlutusData ()
            pure $ mustHaveInput GYTxIn
                    { gyTxInTxOutRef = ref
                    , gyTxInWitness  = GYTxInWitnessScript (GYInScript PlutusV2 script) datum redeemer
                    } <>
                mustHaveOutput GYTxOut
                    { gyTxOutAddress = contractAddr
                    , gyTxOutValue   = valueFrom $ adPrice d
                    , gyTxOutDatum   = Nothing
                    , gyTxOutRefS    = Nothing
                    }

