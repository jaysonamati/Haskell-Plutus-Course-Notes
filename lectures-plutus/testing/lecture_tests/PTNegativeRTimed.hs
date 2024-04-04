{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Control.Monad           (mapM, replicateM)
import qualified NegativeRTimed          as OnChain
import           Plutus.Model            (Ada (Lovelace), DatumMode (HashDatum),
                                          Run, Tx, UserSpend, ada, adaValue,
                                          currentTimeRad, defaultBabbage,
                                          initMock, mustFail, newUser, payToKey,
                                          payToScript, runMock, spend,
                                          spendScript, submitTx, userSpend,
                                          utxoAt, validateIn, valueAt,
                                          waitUntil)
import           Plutus.Model.V2         (TypedValidator, mkTypedValidator)
import           PlutusLedgerApi.V2      (POSIXTime (POSIXTime, getPOSIXTime),
                                          PubKeyHash, TxOut (txOutValue),
                                          TxOutRef, Value)
import           PlutusTx.Builtins       (Integer, mkI)
import           PlutusTx.Prelude        (Bool (..), Eq ((==)), return, ($),
                                          (&&), (.))
import           Prelude                 (IO, Ord ((<), (>)), mconcat, Applicative (pure))
import           Test.QuickCheck         (Arbitrary (arbitrary), Property,
                                          Testable (property), choose, collect,
                                          (==>))
import           Test.QuickCheck.Monadic (assert, monadic, run)
import           Test.Tasty              (defaultMain, testGroup)
import           Test.Tasty.QuickCheck   as QC (testProperty)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

-- | Test the validator script
main :: IO ()
main = defaultMain $ do
    testGroup
        "Testing script properties"
        [ testProperty "Anything before the deadline always fails       " prop_Before_Fails
        , testProperty "Positive redeemer after deadline always fails   " prop_PositiveAfter_Fails
        , testProperty "Negative redeemer after deadline always succeeds" prop_NegativeAfter_Succeeds
        ]


---------------------------------------------------------------------------------------------------
-------------------------------- HELPER FUNCTIONS/INSTANCES ---------------------------------------

-- | Make Run an instance of Testable so we can use it with QuickCheck
instance Testable a => Testable (Run a) where
    property rp = let (a,_) = runMock rp $ initMock defaultBabbage (adaValue 10_000_000) in property a

-- Make POSIXTime an instance of Arbitrary so QuickCheck can generate random values to test
instance Arbitrary POSIXTime where
    arbitrary = do
        n <- choose (0,2_000)  -- we give a low value here since quickcheck might give up in finding random values that pass/fail the test
        pure (POSIXTime n)

-- Time to wait before consumming UTxO from script
waitBeforeConsumingTx :: POSIXTime
waitBeforeConsumingTx = 1_000

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1_000)

valScript :: TypedValidator datum redeemer
valScript = mkTypedValidator OnChain.validator

-- Create transaction that spends "usp" to lock "val" in "giftScript"
lockingTx :: POSIXTime -> UserSpend -> Value -> Tx
lockingTx dl usp val =
  mconcat
    [ userSpend usp
    , payToScript valScript (HashDatum (OnChain.MkCustomDatum dl)) val
    ]

-- Create transaction that spends "giftRef" to unlock "giftVal" from the "valScript" validator
consumingTx :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx dl redeemer usr ref val =
  mconcat
    [ spendScript valScript ref (mkI redeemer) (OnChain.MkCustomDatum dl)
    , payToKey usr val
    ]    


---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------


-- All redeemers fail before deadline
prop_Before_Fails :: POSIXTime -> Integer -> Property
prop_Before_Fails d r = (d > 1_000) ==> runChecks False d r

-- Positive redeemer always fail after deadline
prop_PositiveAfter_Fails :: POSIXTime -> Integer -> Property
prop_PositiveAfter_Fails d r = (r > 0 && d < 1_000) ==> runChecks False d r

-- Negative redeemer always succeed after deadline
prop_NegativeAfter_Succeeds :: POSIXTime -> Integer -> Property
prop_NegativeAfter_Succeeds d r = (r < 0 && d < 1_000) ==> runChecks True d r


---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

-- Check that the expected and real balances match after using the validator with different redeemers
runChecks :: Bool -> POSIXTime -> Integer -> Property
runChecks shouldConsume deadline redeemer = 
    collect (redeemer, getPOSIXTime deadline) $ monadic property check
      where 
        check = do
            balancesMatch <- run $ testValues shouldConsume deadline redeemer
            assert balancesMatch


-- Function to test if both creating and consuming script UTxOs works properly
testValues :: Bool -> POSIXTime -> Integer -> Run Bool
testValues shouldConsume datum redeemer = do
    -- SETUP USERS
    [u1, u2] <- setupUsers
    -- USER 1 LOCKS Lovelaces ("val") IN VALIDATOR
    let val = adaValue 100                -- Define value to be transfered
    sp <- spend u1 val                    -- Get user's UTXOs that we should spend
    submitTx u1 $ lockingTx datum sp val  -- User 1 submits "lockingTx" transaction
    -- WAIT FOR A BIT
    waitUntil waitBeforeConsumingTx
    -- USER 2 TAKES "val" FROM VALIDATOR
    utxos <- utxoAt valScript   -- Query blockchain to get all UTxOs at script
    let [(oRef, oOut)] = utxos  -- We know ther is only one UTxO (the one we created before)
        tx = consumingTx datum redeemer u2 oRef (txOutValue oOut)             -- Define transaction to be submitted
        v2Expected = if shouldConsume then adaValue 1_100 else adaValue 1_000 -- Define expected balance of user 2
    ct <- currentTimeRad 100  -- Create time interval with equal radius around current time
    tx' <- validateIn ct tx   -- Build final consuming tx
    if shouldConsume then submitTx u2 tx' else mustFail . submitTx u2 $ tx'   -- User 2 submits "consumingTx" transaction
    -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
    [v1, v2] <- mapM valueAt [u1, u2]               -- Get final balances
    return $ v1 == adaValue 900 && v2 == v2Expected -- check if final balances match expected balances

