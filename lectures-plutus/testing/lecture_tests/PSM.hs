{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import           Prelude
import           Test.Tasty         (defaultMain, testGroup)

import           Control.Monad      (replicateM, unless)
import           Plutus.Model       (Ada (Lovelace), Run, ada, adaValue,
                                     defaultBabbage, mustFail, newUser,
                                     sendValue, testNoErrors, valueAt, logError)
import           PlutusLedgerApi.V1 (PubKeyHash)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
    testGroup
        "Test simple user transactions"
        [ good "Simple spend" simpleSpend
        , bad  "Not enough funds" notEnoughFunds
        ]
        where
            bad msg = good msg . mustFail
            good = testNoErrors (adaValue 10_000_000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setUpUsers :: Int -> Run [PubKeyHash]
setUpUsers n = replicateM n $ newUser $ ada (Lovelace 1_000)

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING TRANSACTIONS ----------------------------------------

-- Function to test that a simple transaction works
simpleSpend :: Run ()
simpleSpend = do
    users <- setUpUsers 3           -- Create 3 users and assign each 1000 lovelace
    let [u1, u2, u3] = users        -- Give names to individual users
    sendValue u1 (adaValue 100) u2  -- Send 100 lovelace from user 1 to user 2
    sendValue u2 (adaValue 100) u3  -- Send 100 lovelace from user 2 to user 3
    vals <- mapM valueAt users      -- Read user values
    unless (vals == fmap adaValue [900, 1_000, 1_100]) $ 
        logError "values don't match"


-- Function to test that a transaction fails if there are not enough funds
notEnoughFunds :: Run ()
notEnoughFunds = do
    users <- setUpUsers 3
    let [u1, u2, _] = users
    sendValue u1 (adaValue 10_000) u2 -- Send 10.000 lovelaces from user 1 to user 2

