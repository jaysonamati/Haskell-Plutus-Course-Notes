module Server.Chat
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


type Name = String

data Message = 
      Connected Name
    | Disconnected Name
    | Tell Name Name String
    | Kick Name Name
    | Quit
    | Message Name String
    deriving Show

run :: IO ()
run = listen (Host "127.0.0.1") "8765" $ \(socket, addr) -> do
    putStrLn $ "listening on: " ++ show addr
    names <- newTVarIO []
    ch    <- newBroadcastTChanIO
    forever $
        void $ accept socket $ \(socket', addr') -> do
            putStrLn $ "accepted client: " ++ show addr'
            name <- newTVarIO Nothing
            forkFinally
                (withSocket socket' handleClient name names ch)
                (const $ do 
                    atomically $ do
                        m <- readTVar name
                        case m of
                            Nothing -> pure ()
                            Just n  -> 
                                modifyTVar' names $ filter (/= n)
                                writeTChan ch $ Disconnected n
                    putStrLn $ "client disconnected " ++ show addr')  -- this could be any IO action... (writeToFile, writeTODb) 

handleClient :: TVar (Maybe Name) 
             -> TVar [Name] 
             -> TChan Message 
             -> WriteLine 
             -> ReadLine -> IO ()
handleClient name names ch writeLine readLine = do
    ch' <- atomically $ dupTChan ch -- evry client has it's own duplicate channel, so they can 'monitor' it... 
    n   <- pickName writeLine readLine name names
    atomically $ writeTChan ch $ Connected n
    hSetBuffering h LineBuffering
    void $ input n `race` output ch' n
  where
    input n = go
        where
            go = do
                s <- readLine
                case parseUserInput n s of
                    Quit -> pure ()
                    msg  -> do
                        atomically $ writeTChan ch msg
                        go

    output ch' n = go
        where
            go = do
                msg <- atomically $ readTChan ch'
                case msg of
                    Connected m 
                        | m /= n -> do 
                            writeLine $ "***" ++ m ++ " connected" 
                            go
                    Disconnected m -> do 
                        writeLine $ "***" ++ m ++ " disconnected"
                        go
                    Message m msg'
                        | m /= n -> do
                            writeLine $ m ++ " >>> " ++ msg'
                            go
                    Tell who whom msg'
                        | whom == n && who /= n -> do
                            writeLine $ who ++ " DM: " ++ msg'
                            go
                    Kick who whom
                        | whom == n && who /= n ->
                            writeLine $ who ++ " kicked you!!!"
                    _ -> go

pickName :: WriteLine 
         -> ReadLine
         -> TVar (Maybe Name)
         -> TVar [Name]       -- all names 
         -> IO Name
pickName writeLine readLine name names = go
  where
    go = do
        writeLine "Please choose a nickname!"
        n <- readLine
        if n == ""
            then writeLine "Please don't use the empty name!" >> go
            else do
                b <- atomically $ do
                    ns <- readTVar name
                    if (n `elem` ns)
                        then pure False
                        else do
                            modifyTVar' names (n :)
                            writeTVar name $ Just n
                            pure True
                if b
                    then pure n
                    else writeLine "Name already taken! " >> go


parseUserInput :: Name -> String -> Message
parseUserInput _ "/quit" = Quit
parseUserInput n s       = case words s of
    ("/tell": m: msg) -> Tell n m $ unwords msg
    ["/kick", m]      -> Kick n m
    _                 -> Message n s