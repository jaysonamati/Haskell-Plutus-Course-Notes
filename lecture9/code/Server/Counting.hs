module Server.Counting
    (run
    ) where 

import           Control.Monad      (forever, void)
import           Control.Concurrent 
import           Control.Concurrent.STM
import           Network.Simple.TCP (HostPreference (Host), acceptFork, listen)
import           Network.Socket     (socketToHandle)
import           System.IO          (BufferMode (LineBuffering), Handle,
                                     IOMode (ReadWriteMode), hGetLine,
                                     hPutStrLn, hSetBuffering)

import           Server.Utils                                     

run :: IO ()
run = listen (Host "127.0.0.1") "8765" $ \(socket, addr) -> do
    putStrLn $ "listening on: " ++ show addr
    conns <- newTVarIO []
    void $ forkIO $ monitor 0 conns
    forever $
        void $ accept socket $ \(socket', addr') -> do
            putStrLn $ "accepted client: " ++ show addr'
            forkFinally
                (withSocket socket' handleClient conns socket')
                (const $ do 
                    atomically $ modifyTVar' conns $ filter (\(s, _) -> s /= socket)
                    putStrLn $ "client disconnected " ++ show addr')  -- this could be any IO action... (writeToFile, writeTODb) 

handleClient :: TVar [(Socket, WriteLine)] -> Socket -> WriteLine -> ReadLine -> IO ()
handleClient conns socket writeLine readLine = do
    atomically $ modifyTVar' conns ((socket, writeLine) :)
    hSetBuffering h LineBuffering
    forever $ void readLine


monitor :: Int -> TVar [(Socket, WriteLine)] -> IO ()
monitor count conns = do
    (writes, newCount) <- atomically $ do
        xs <- readTVar conns
        let newCount = length xs
        if (newCount == count)
            then retry
            else pure (snd <$> xs, newCount)
    forM_ writers $ \writeLine ->
        writeLine $ "Number of clients changed to: " ++ show newCount
    monitor newCount conns