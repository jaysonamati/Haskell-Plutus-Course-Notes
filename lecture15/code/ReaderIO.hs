module ReaderIO where

import Control.Monad.Reader (ReaderT (..), MonadReader (..), MonadIO (liftIO))
import Control.Monad (forM_)

type M = ReaderT FilePath IO

runM :: M a -> FilePath -> IO a
runM = runReaderT

writeLog :: String -> M ()
writeLog msg = do
    logFile <- ask
    liftIO $ appendFile logFile (msg ++ "\n")
    -- liftIO $withFile logFile AppendMode $ \h ->
    --     hPutStrLn h (msg ++ "\n")

exampleProg :: M ()
exampleProg = forM_ [1 .. 100 :: Int] $ \n ->
    writeLog $ show n