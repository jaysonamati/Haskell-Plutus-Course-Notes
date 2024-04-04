{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Vesting where

import qualified PlutusLedgerApi.V2 as PlutusV2
import           PlutusLedgerApi.V2 (POSIXTime, PubKeyHash, TxInfo (..), ScriptContext (scriptContextTxInfo))
import           PlutusTx           (BuiltinData, CompiledCode, compile)
import qualified PlutusTx
import           PlutusTx.Prelude   (Bool,traceIfFalse,(&&), elem)
import qualified Prelude                          
import           Prelude            (IO)          
import           Utilities          (wrapValidator, writeValidatorToFile)
import           PlutusLedgerApi.V1.Interval  (contains, from)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------


-- Vesting: Only unlocks at a specific time and only when signed by a specific PKH

data VestingDatum = VestingDatum
    { vdDeadline    :: POSIXTime
    , vdBeneficiary :: PubKeyHash
    } deriving Prelude.Show

PlutusTx.unstableMakeIsData ''VestingDatum

mkVestingValidator :: VestingDatum -> () -> PlutusV2.ScriptContext -> Bool
mkVestingValidator VestingDatum {..} () ctx = 
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

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "../../assets/vesting.plutus" vestingValidator