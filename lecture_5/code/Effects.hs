module Effects where

import Control.Monad ()
import Data.Char
import System.IO

main :: IO ()
main = putStrLn "Hello Friend!"

shout :: IO String
shout = map toUpper <$> getLine

foo :: IO (String -> String)
foo = (++) <$> getLine

joinTwoLines :: IO String
joinTwoLines = liftM2 (++) getLine getLine
flipTwoLines = liftM2 (flip (++)) getLine getLine

shoutBack :: IO String
shoutBack = shout >>= putStrLn

shoutBackTwice :: IO ()
shoutBackTwice = shout >>= \x -> putStrLn x >> putStrLn x

liftM :: (a -> b) -> IO a
liftM f x = x >>= \a -> return f a

liftM2 :: (a -> b -> c) -> IO a -> IO b -> IO c
liftM2 f x y = 
    x >>= \a ->
    y >>= \b ->
    return (f a b)

liftM2' :: (a -> b -> c) -> IO a -> IO b -> IO c
liftM2' f x y = do
    a <- x
    b <- y
    return (f a b)

greeting :: IO ()
greeting = 
    putStrLn "What's your name ?" >>
    getLine                       >>= \name ->
    putStrLn ("Hello, " ++ name ++ " !") >>
    putStrLn "Where do you live ?"       >>
    getLine                              >>= \loc ->
        let
            answer
                | loc == "Nairobi" = "Fantastic!"
                | loc == "Nakuru"  = "Outstanding!"
                | otherwise        = "Never heard of it!"
        in putStrLn answer


ask :: String -> IO String
ask question = do
    putStrLn question
    getLine

sequence :: [IO a] -> IO [a]
sequence [] = return []
sequence (x:xs) = do
    y  <- x
    ys <- sequence xs
    return $ y : ys

mapM :: (a -> IO b) -> [a] -> IO [b]
mapM f = sequence . map  f

askMany :: [String] -> IO [String]
askMany []      = return []
askMany (q: qs) = do 
    x  <- ask q
    xs <- askMany qs
    return (answer : answers)

askMany' :: [String] -> IO [String]
askMany' = sequence . map ask    

askMany'' :: [String] -> IO [String]
askMany'' = mapM ask


data Interaction = 
      Question String Interaction Interaction
    | Result String


pick :: Interaction
pick = 
    Question "Do you like functional programming?"    
        (Question "Do you like static types?"
            (Result "Try Haskell")
            (Result "Try Clojure"))
        (Question "Do you like dynamic types?"
            (Result "Try Python")
            (Result "Try Rust"))

ford :: Interaction
ford = Question "Would you like a car?"            
            (Result "Do you like it in black?"
                (Result "Good for you.")
                ford)
            (Result "Never mind then!")

askBool :: String -> IO Bool
askBool question = do
    putStrLn $ question ++ " [yn]"
    x <- getChar
    putStrLn ""
    return $ x `elem` "yY"

interaction :: Interaction -> IO ()
interaction (Result r) = putStrLn r
interaction (Question question yes no) = do
    b <- askBool question
    interaction $ if b then yes else no


simulate :: Interaction -> [Bool] -> Maybe String
simulate (Result r) _               = Just r
simulate (Question question yes _) (True : bs)   = simulate yes bs
simulate (Question _ _ no)          (False : bs) = simulate no bs
simulate                   []         = Nothing


readFileLineByLine :: FilePath -> IO [String]
readFileLineByLine file = withFile file ReadMode $ readFileHandle

readFileHandle :: Handle -> IO [String]
readFileHandle handle = do -- this only covers the first 'function scope'
    b <- hIsEOF handle
    if b 
        then return []
        else do
            line <- hGetLine handle
            otherLines <- readFileHandle handle
            return (line : otherLines)


readFileLineByLine' :: FilePath -> IO (Maybe [String])
readFileLineByLine' file = catchIOError
    (Just <$> readFileLineByLine' file) $ const $ return Nothing



-- Getting system args

parseArgs :: IO FilePath
parseArgs = do
    xs <- getArgs
    case xs of
        [file] -> return file
        _      -> ioError $ userError "Expected exactly one command line argument"
            

main :: IO ()            
main = do 
    file   <- parseArgs
    mlines <- readFileLineByLine' file
    print mlines