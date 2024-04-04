module Server.Shouting where

import           Control.Monad      (forever, void)
import           Control.Concurrent
import           Data.Char          (toUpper)
import           Network.Simple.TCP (HostPreference (Host), acceptFork, listen)
import           Network.Socket     (socketToHandle)
import           System.IO          (BufferMode (LineBuffering), Handle,
                                     IOMode (ReadWriteMode), hGetLine,
                                     hPutStrLn, hSetBuffering)

import           Server.Utils                                     

main :: IO ()
main = listen (Host "127.0.0.1") "8765" $ \(socket, addr) -> do
    putStrLn $ "listening on: " ++ show addr
    forever $
        void $ accept socket $ \(socket', addr') -> do
            putStrLn $ "accepted client: " ++ show addr'
            forkFinally
                (withSocket socket' handleClient)
                (putStrLn $ "client disconnected " ++ show addr')  -- this could be any IO action... (writeToFile, writeTODb) 

handleClient :: WriteLine -> ReadLine -> IO ()
handleClient writeLine readLine = do
    hSetBuffering h LineBuffering
    forever $ do
        line <- readLine
        writeLine $ toUpper <$> line