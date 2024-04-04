module Parser.Five where

import Control.Monad
import Control.Applicative
import Data.List
import Numeric.Natural

newtype Parser t a = Parser { runParser :: [t] -> [(a, [t])]}


instance Functor (Parser t) where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \s ->
        [(f a, t) | (a, t) <- runParser p s]

instance Applicative (Parser t) where

    pure :: a -> Parser t a
    pure a = Parser $ \s -> [(a,s)]

    (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b -- "asd4ffad"
    p <*> q = Parser $ \s -> do
        (f, t) <- runParser p s
        (a, u) <- runParser q t
        pure (f a , u)

instance Alternative (Parser t) where

    empty :: Parser t a
    empty = Parser $ \s -> t

    (<|>) :: Parser t a -> Parser t a -> Parser a
    p <|> q = Parser $ \s -> runParser p s ++ runParser q s

instance Monad (Parser t) where

    (>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
    p >>= k = Parser $ \s -> do
        (a, t) <- runParser p s
        runParser (k a) t

instance MonadPlus (Parser t) where
    mzero = empty
    mplus = (<|>)

eof :: Parser ()
eof = Parser $ \s -> case s of
    []          -> [((),[])]
    _ : _       -> []

satisfy :: (t -> Bool) -> Parser t
satisfy p       = Parser $ \s -> case s of
    []              -> []
    c : cs
        | p c       -> [(c,cs)]
        | otherwise -> []

digit :: Parser Char Char
digit = satisfy (`elem` "0123456789")

letter :: Parser Char Char
letter = satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'])

-- char :: Char -> Parser ()
-- char c = void $ satisfy (== c)

token :: Eq t => t -> Parser t ()
token c = () <$ satisfy (== c)

digit' :: Parser Char Natural
digit' = charToNat <$> digit

nzDigit :: Parser Char Natural
nzDigit = charToNat <$> satisfy (`elem`)

charToNat :: Char -> Natural
charToNat c = fromIntergral (fromEnum c) - 48

natural :: Parser Char Natural
natural = 
        (0 <$ token '0')
    <|> (f <$> nzDigit <*> many digit')
  where
    f :: Natural -> [Natural] -> Natural
    f = foldl' (\acc c -> 10 * acc + d)

integer = f <$> optional sign <*> natural
  where
    f :: Maybe (Natural -> Integer) -> Natural -> Integer
    f Nothing = toInteger
    f (Just s)  = s

    sign :: Parser (Natural -> Integer)
    sign = 
            (toInteger        <$ token '+')
        <|> (negate . toInteger <$ token '-')


int :: Parser Char Int
int = do
    n <- integer
    let minB = minBound :: Int
        maxB = maxBound :: Int
    guard $ n >= toInteger minB && n <= toInteger maxB
    pure $ fromIntegral n