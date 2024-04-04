module Server.Counting
    (run
    ) where 

import           Control.Monad      (forever, void)
import           Control.Concurrent
import           Control.Concurrent.Async
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
    conns <- newTVarIO 0
    ch    <- newBroadcastTChanIO
    void $ forkIO $ monitor ch 0 conns
    forever $
        void $ accept socket $ \(socket', addr') -> do
            putStrLn $ "accepted client: " ++ show addr'
            forkFinally
                (withSocket socket' handleClient conns socket')
                (const $ do 
                    atomically $ modifyTVar' conns $ \x -> x - 1 
                    putStrLn $ "client disconnected " ++ show addr')  -- this could be any IO action... (writeToFile, writeTODb) 

handleClient :: TVar Int -> TChan Int -> WriteLine -> ReadLine -> IO ()
handleClient conns ch writeLine readLine = do
    ch' <- atomically $ dupTChan ch -- evry client has it's own duplicate channel, so they can 'monitor' it... 
    atomically $ modifyTVar' conns (+ 1)
    hSetBuffering h LineBuffering
    void $ input `race` output ch'
  where
    input = forever readLine
    output ch' = forever $ do
        newCount <- atomically $ readTChan ch'
        writeLine $ "Number of clients changed to: " ++ show newCount


monitor :: TChan Int -> Int -> TVar Int -> IO ()
monitor ch count conns = do
    newCount <- atomically $ do
        c <- readTVar conns
        let newCount = length xs
        if (c == count)
            then retry
            else do
                writeTChan ch c
                pure c
    monitor ch newCount conns