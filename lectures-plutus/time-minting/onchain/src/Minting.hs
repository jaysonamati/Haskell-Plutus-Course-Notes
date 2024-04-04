{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Minting where

import           PlutusCore.Version (Version (..))
import qualified PlutusLedgerApi.V2 as PlutusV2
import           PlutusLedgerApi.V2 (POSIXTime, PubKeyHash, TxInfo (..), ScriptContext (..), CurrencySymbol, ScriptPurpose (..))
import           PlutusTx           (BuiltinData, CompiledCode, compile, liftCode, unsafeApplyCode, UnsafeFromData (..), liftCodeDef)
import qualified PlutusTx
import           PlutusTx.Prelude   (Bool (..),traceIfFalse,(&&), elem, error, ($), all, otherwise, Eq (..), encodeUtf8, check)
import qualified Prelude                          
import           Prelude            (IO)          
import           Utilities          (wrapValidator, writeValidatorToFile, wrapPolicy)
import           PlutusLedgerApi.V1.Interval  (contains, from)
import PlutusLedgerApi.V1.Value (flattenValue, TokenName (..))

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / MINTING-POLICY ------------------------------------------


-- Vesting: Only unlocks at a specific time and only when signed by a specific PKH

data VestingDatum = VestingDatum
    { vdDeadline    :: POSIXTime
    , vdBeneficiary :: PubKeyHash
    } deriving Prelude.Show

PlutusTx.unstableMakeIsData ''VestingDatum

-- Minting and burning should only be allowed if a specific party signs the transaction
mkTypedMintingPolicy :: PubKeyHash -> () -> PlutusV2.ScriptContext -> Bool
mkTypedMintingPolicy pkh () ctx = 
    traceIfFalse "Signature missing" hasSignature && 
    traceIfFalse "wrong token name" correctTokenName
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName 
    tn = TokenName $ encodeUtf8 "Kenya"

    {-
    hasSignature :: Bool
    hasSignature = "515bb76c61c7da2d2c70ed99940fc33cc89b7b32072838009d014aec" 
        `elem` txInfoSignatories info
    -}
      
    hasSignature :: Bool
    hasSignature = pkh `elem` txInfoSignatories info

    correctTokenName :: Bool
    correctTokenName = all f $ flattenValue $ txInfoMint info
      where
          f :: (CurrencySymbol, PlutusV2.TokenName, Prelude.Integer) -> Bool
          f (cs', tn', _)
              | cs' == cs = tn' == tn
              | otherwise = False

    cs :: CurrencySymbol
    cs = case scriptContextPurpose ctx of
            Minting cs' -> cs'
            _           -> error ()


{-# INLINABLE mkTypedMintingPolicy #-}

mkMintingPolicy :: PubKeyHash -> CompiledCode (BuiltinData -> BuiltinData -> ())
mkMintingPolicy pkh = $$(PlutusTx.compile [|| wrappedMintingPolicy ||]) `unsafeApplyCode`
    liftCode (Version 1 0 0) pkh
  where

    wrappedMintingPolicy :: PubKeyHash -> BuiltinData -> BuiltinData -> ()
    wrappedMintingPolicy pkh' r ctx = check $ 
        mkTypedMintingPolicy pkh' (unsafeFromBuiltinData r) (unsafeFromBuiltinData ctx)

{-mkVestingValidator VestingDatum {..} () ctx = 
    traceIfFalse "deadline not reached" deadlineReached &&
    traceIfFalse "beneficiary did not sign" beneficiarySigned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    deadlineReached :: Bool
    deadlineReached = from vdDeadline `contains` txInfoValidRange info 

    beneficiarySigned :: Bool
    beneficiarySigned = vdBeneficiary `elem` txInfoSignatories info
{-# INLINABLE mkVestingValidator #-}

vestingValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
vestingValidator = $$(PlutusTx.compile [|| wrappedVestingValidator ||])
  where
    wrappedVestingValidator = wrapValidator mkVestingValidator
-}
---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------
{-
saveVal :: IO ()
saveVal = writeValidatorToFile "../../assets/vesting.plutus" vestingValidator
-}