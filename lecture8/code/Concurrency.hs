module Concurrency where


import Control.Monad
import Control.Concurrent (forkIO, threadDelay)

-- a mini exercise

numberForever :: Int -> IO ()
numberForever n = forever $ print n

numberForever' :: Int -> IO a
numberForever' n = do
    print n
    numberForever' n


numberForever'' :: Int -> IO ()
numberForever'' n = go
    where
        go = print n >> go


second :: Int
second = 1000000

thread :: Int -> IO a
thread n = forever $ do
    print n
    threadDelay $ second `div` 10

main :: IO ()
main = do
    mapM_ (forkIO . thread) [1 .. 10]
    threadDelay $ 5 * second
