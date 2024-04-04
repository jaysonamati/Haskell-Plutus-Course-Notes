{-# LANGUAGE RecordWildCards #-}

module AbstractionPatterns where

-- Monad and Applicative stuff...!

import qualified Table as T

type Bank = Table String Natural

data Tx = Tx {
      txFrom :: String
    , txTo   :: String
    , txAmt  :: Natural
    } deriving (Show)


processTx :: Tx -> Bank -> Maybe Bank
processTx tx bank
    | txFrom == txTo || txAmt == 0 = Nothing
    | otherwise
        case T.lookup txFrom bank of
            Nothing -> Nothing
            Just oldFrom
                | oldFrom < txAmt -> Nothing
                | otherwise
                    case T.lookup txTo bank of
                        Nothing -> Nothing
                        Just oldTo -> 
                            let newFrom = oldFrom - txAmt
                                newTo   = oldTo + txAmt
                            in
                                Just $ T.insert txFrom newFrom $
                                       T.insert txTo newTo bank


bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- bindMaybe m f = case m of
--     Nothing -> Nothing
--     Just x  -> f x
bindMaybe m f = maybe Nothing f m

bindEither :: Either e a -> (a -> Either e b) -> Either e b
bindEither x f = case x of
    Left err -> Left err
    Right a  -> f a

lookupE :: String -> String -> Bank -> Either String Natural
lookupE msg account bank = case T.lookup account bank of
    Nothing  -> throwError msg
    Just amt -> Right amt


checkMaybe :: Bool -> Maybe a -> Maybe a
checkMaybe b m 
    | b         = Nothing
    | otherwise = m 

processTx' :: Tx -> Bank -> Maybe Bank
processTx' tx bank
    | txFrom == txTo || txAmt == 0 = Nothing
    | otherwise
        T.lookup txFrom bank `bindMaybe` \oldFrom -> 
            if oldFrom < txAmt
                then Nothing
                else T.lookup txTo bank `bindMaybe` \ oldTo ->
                    let newFrom = oldFrom - txAmt
                        newTo   = oldTo + txAmt
                    in
                        Just $ T.insert txFrom newFrom $
                               T.insert txTo newTo bank

throwError :: String -> Either String a
throwError = Left

processTxE :: Tx -> Bank -> Either String Bank
processTxE tx bank
    | txFrom == txTo = throwError "sender equals received"
    | txAmt == 0     = throwError "zero amount"
    | otherwise      =
        T.lookup txFrom bank `bindEither` \ oldFrom
            Nothing -> throwError "unknown sender"
            Just oldFrom
                | oldFrom < txAmt -> throwError "insufficient funds, missing " ++ show (txAmt -oldFrom)
                | otherwise
                    case T.lookup txTo bank of
                        Nothing -> throwError "unknown receiver"
                        Just oldTo -> 
                            let newFrom = oldFrom - txAmt
                                newTo   = oldTo + txAmt
                            in
                                Right $ T.insert txFrom newFrom $
                                       T.insert txTo newTo bank


processE' :: Tx -> Bank -> Either String Bank
processE' tx bank
    | txFrom == txTo = throwError "sender equals received"
    | txAmt == 0     = throwError "zero amount"
    | otherwise      =
        lookupE "unknown sender" txFrom bank `bindEither` \ oldFrom
                  if oldFrom < txAmt
                  then throwError "insufficient funds, missing " ++ show (txAmt -oldFrom)
                  else 
                    lookupE "unknown receiver" txTo bank \oldTo 
                            let newFrom = oldFrom - txAmt
                                newTo   = oldTo + txAmt
                            in
                                Right $ T.insert txFrom newFrom $
                                       T.insert txTo newTo bank

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show


label :: Tree a -> Tree (Int, a)
lable t = snd $ label' 1 t


label' :: Int -> Tree a -> (Int, Tree (Int, a))
label' nextLabel (Leaf a) = (nextLabel + 1, Leaf (nextLabel, a))
label' nextLabel (Node l r) = 
    let
        (newNextLabel, l')       = label' nextLabel 1
        (evenNewerNextLabel, r') = label' newNextLabel 1
    in 
        (evenNewerNextLabel, Node l' r')


-- type State s a = s -> (s, a)
newtype State s a = State{ runState :: s -> (s, a) }

bindState :: State s a -> (a -> State s b) -> State s b
bindState s k = \oldState
    let 
        (updatedState, a) = s oldState
    in
        k a updatedState


get :: State s s
get = \currentState -> (currentState, currentState)

put :: s -> State s ()
put newState = const (newState, ())

pureS :: a -> State s a
pureS a = \s -> (s, a)


labelS :: Tree a -> State Int (Tree (Int, a))
labelS (Leaf a) = 
    get                  `bindState` \nextLabel ->
    put (nextLabel + 1)  `bindState` 



bindList :: [a] -> (a -> [b]) -> [b]
bindList = flip concatMap
-- Given a list of 

wordLengths :: [String] -> [Int]
wordLengths lines = 
    lines      `bindList` \line -> 
    words line `bindList` \word ->
    [length word]

wordLengths :: [String] -> [Int]
wordLengths lines' = 
    line <- lines'
    word <- words line
    pure $ length word

wordLengths'' :: [String] -> [Int]
wordLengths'' lines' = 
    [length lines | line <- lines' , word <- words line ]


class Functor m => Applicative m where
    pure  :: a -> m a
    (<*>) :: m (a -> b) -> m a -> m b

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
    f <- mf
    a <- ma
    pure $ f a

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b


when, unless :: Applicative m => Bool -> m () -> m ()
when True  action  = action
when False _       = pure ()

unless b action    = when (not b)

evalState = fst runState


-- Usefule in config files 
newtype REader r a = Reader {runReader :: r -> a}

ask :: Reader r r
ask = Reader id

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure a = Reader $ const a 
    (<*>) = ap

instance Monad (Reader r) where
    ma >>= k = Reader $ \s -> 
        let 
            a = runReader ma s
        in
            runReader (k a) s 


labelRose :: Rose a -> Rose (Int, a)
labelRose t = evalState (labelRoseS t) 1


labelRoseS :: Rose a -> State (Int, Rose (Int, a))
labelRoseS (Fork a xs) = do
    l <- get
    put $ l + 1
    let a' = (l, a)
    xs' <- mapM labelRoseS xs
    pure $ Fork a' xs'

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    pure $ f s

-- >> filterM (\n -> if even n then putStrLn (show n ++ " even"))