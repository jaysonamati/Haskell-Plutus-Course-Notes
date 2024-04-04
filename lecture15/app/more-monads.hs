import System.Environment (getArgs)
import ReaderIO

main :: IO ()
main = do
    [logFile] <- getArgs
    runM exampleProg logFile