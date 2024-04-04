module Server.Utils
    ( WriteLine
    , ReadLine
    , withSocket
    ) where

import Network.Simple.TCP
import Network.Socket
import System.IO


type WriteLine = String -> IO ()
type ReadLine = IO String

withSocket :: Socket
           -> (WriteLine -> ReadLine -> IO ())    
           -> IO ()
withSocket socket handler = do
    h <- socketToHandle socket ReadWriteMode
    hSetBuffering h LineBuffering
    handler (hPutStrLn h) (hGetLine h) 
        `catchIOError`
            (const $ pure ())