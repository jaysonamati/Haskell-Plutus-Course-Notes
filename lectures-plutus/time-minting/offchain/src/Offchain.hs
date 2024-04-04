{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Offchain where

import           GeniusYield.TxBuilder (GYTxMonad, GYTxSkeleton, mustHaveOutput, GYTxQueryMonad, scriptAddress, runGYTxMonadNode)
import           GeniusYield.Types (GYValue, GYTime, GYPubKeyHash, GYTxOut (..), PlutusVersion (..), timeToPlutus, pubKeyHashToPlutus, datumFromPlutusData, GYTxOutUseInlineDatum (..), GYValidator, GYAddress, validatorFromPlutus, GYTxId, readPaymentSigningKey, paymentVerificationKey, pubKeyHash, addressFromPubKeyHash, signGYTxBody, GYProviders (gySubmitTx), getCurrentGYTime)
import qualified Vesting
import GeniusYield.GYConfig (coreConfigIO, GYCoreConfig (cfgNetworkId), withCfgProviders)

vestingValidator :: GYValidator 'PlutusV2
vestingValidator = validatorFromPlutus Vesting.vestingValidator

vestingAddress :: GYTxQueryMonad m => m GYAddress
vestingAddress = scriptAddress vestingValidator

placeVesting :: GYTxMonad m => GYValue -> GYTime -> GYPubKeyHash -> m (GYTxSkeleton 'PlutusV2)
placeVesting v deadline beneficiary = do
    addr <- vestingAddress
    let d = Vesting.VestingDatum
                { Vesting.vdDeadline    = timeToPlutus deadline
                , Vesting.vdBeneficiary = pubKeyHashToPlutus beneficiary
                }
    pure $ mustHaveOutput GYTxOut
        { gyTxOutAddress = addr
        , gyTxOutDatum   = Just (datumFromPlutusData d, GYTxOutUseInlineDatum)
        , gyTxOutValue   = v
        , gyTxOutRefS    = Nothing
        }

{-
placeVestingIO :: FilePath -> FilePath -> GYValue -> GYTime -> GYPubKeyHash -> IO GYTxId
placeVestingIO cfgFile skeyFile v deadline beneficiary = do
    cfg <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    now <- getCurrentGYTime
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Vesting" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (placeVestingIO v deadline beneficiary)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx
-}