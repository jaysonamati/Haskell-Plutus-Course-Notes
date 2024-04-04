{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module FreeMonads.Concurrency where

import FreeMonads.Free (Free (..))
import Control.Monad (replicateM_)

data ProcessOp r where
    Atomically :: IO a -> (a -> r) -> ProcessOp r
    Fork       :: Process () -> r -> ProcessOp r

deriving instance Functor ProcessOp

{-
a is an existential type
-}

type Process = Free ProcessOp

atomically :: IO a -> Process a
atomically m = Free $ Atomically m pure

fork :: Process () -> Process ()
fork p = Free $ Fork p $ pure ()

program :: Process ()
program = do
    fork $ replicateM_ 5 $ atomically $ putStrLn "Haskell"
    fork $ replicateM_ 7 $ atomically $ putStrLn "Nairobi"
    replicateM_ 3 $ atomically $ putStrLn "Kenya"

schedule :: [Process ()] -> IO ()
schedule []                            = pure ()
schedule (Pure () : ps)                = schedule ps
schedule (Free (Atomically io k) : ps) = do 
    a <- io
    schedule $ ps ++ [k a] -- attach the continuation at the end of the other threads
schedule (Free (Fork p q) : ps)        = schedule $ ps ++ [q, p] -- schedule the newly forked thread at the end