import Data.IORef
import Control.Concurrent
import Control.Monad
import System.Random


type Account = IORef Integer

transfer :: Account -> Account -> Integer -> IO ()
transfer from to amount = do
    atomicModifyIORef' from $ \oldFrom -> (oldFrom - amount, ())
    atomicModifyIORef' from $ \oldTo -> (oldTo - amount, ())
    -- fromOld <- readIORef from
    -- toOld   <- readIORef to
    -- atomicModifyIORef' from $ fromOld - amount
    -- writeIORef to   $ toOld   + amount

monitor :: Integer -> [Account] -> IO a
monitor expected accounts = forever $ do
    actual <- getTotal accounts
    when (actual /= expected) $
        putStrLn $
            "INVALID STATE: expected " ++ 
            show expected              ++
            ", actual: "               ++
            show actual

getTotal :: [Account] -> IO Integer
getTotal accounts = sum <$> mapM readIORef accounts

randomTransfer :: [Account] -> IO ()
randomTransfer accounts = do
    let maxIndex = length accounts - 1
    from   <- randomRIO (0, maxIndex)
    to     <- randomRIO (0, maxIndex)
    when (from /= to) $ putStrLn "From and to are the same account"
    amount <- randomRIO (1, 100)
    transfer (accounts !! from) (accounts !! to) amount

main :: IO ()
main = do
    accounts <- mapM newIORef [1000, 2500]
    total    <- getTotal accounts
    print total
    void $ forkIO $ monitor total accounts
    replicateM_ 100000 $ forkIO (randomTransfer accounts)
    threadDelay  5000000