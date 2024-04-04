import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random


type Account = TVar Integer

transfer :: Account -> Account -> Integer -> STM (IO ()) -- this is for priniting io
transfer from to amount = do
    oldFrom <- readTVar from
    if (oldFrom < amount) 
        then pure $ putStrLn "Not okay!"
        else do
            modifyTVar from (\x -> x - amount)
            modifyTVar to (+ amount)
            pure $ putStrLn $ "okay: " ++ show amount

-- using the `retry :: STM a`
transfer' :: Account -> Account -> Integer -> STM () --- STM ((IO ()))
transfer' from to amount = do
    oldFrom <- readTVar from
    if (oldFrom < amount) 
        then pure () -- retry
        else do
            modifyTVar from (\x -> x - amount)
            modifyTVar to (+ amount)
            -- pure $ putStrLn $ "okay: " ++ show amount            

monitor :: Integer -> [Account] -> IO a
monitor expected accounts = forever $ do
    actual <- atomically $ getTotal accounts
    when (actual /= expected) $
        putStrLn $
            "INVALID STATE: expected " ++ 
            show expected              ++
            ", actual: "               ++
            show actual

getTotal :: [Account] -> STM Integer
getTotal accounts = sum <$> mapM readTVar accounts

randomTransfer :: [Account] -> TVar Bool ->  IO ()
randomTransfer accounts done = do
    let maxIndex = length accounts - 1
    from   <- randomRIO (0, maxIndex)
    to     <- randomRIO (0, maxIndex)
    when (from /= to) $ putStrLn "From and to are the same account"
    amount <- randomRIO (1, 100)
    join $ atomically $ do 
        transfer' (accounts !! from) (accounts !! to) amount
        writeTVar done True


-- main :: IO ()
-- main = do
--     accounts <- mapM newTVarIO [1000, 2500]
--     total    <- atomically $ getTotal accounts
--     print total
--     void $ forkIO $ monitor total accounts
--     replicateM_ 100000 $ 
--         forkIO (randomTransfer accounts)
--     threadDelay  5000000

main :: IO ()
main = do
    accounts <- mapM newTVarIO [1000, 2500]
    total    <- atomically $ getTotal accounts
    print total
    bs <- replicateM 100000 $ newTVarIO False
    void $ forkIO $ monitor total accounts
    forM_ bs $ \b -> 
        forkIO (randomTransfer accounts b)
    atomically $ do
        xs <- mapM readTVar bs
        unless (and xs)
            retry


transfer'' :: Account -> Account -> Account -> Integer -> STM (IO ())
transfer'' from from' to amount = 
    transfer' from to amount `orElse`
    transfer' from' to amount