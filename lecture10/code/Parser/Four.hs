module Parser.Three where

import Control.Monad
import Control.Applicative
import Data.List

newtype Parser a = Parser { runParser :: String -> [(a, String)]}


instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \s ->
        [(f a, t) | (a, t) <- runParser p s]

instance Applicative Parser where

    pure :: a -> Parser a
    pure a = Parser $ \s -> [(a,s)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    p <*> q = Parser $ \s -> do
        (f, t) <- runParser p s
        (a, u) <- runParser q t
        pure (f a , u)

instance Alternative Parser where

    empty :: Parser a
    empty = Parser $ \s -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = Parser $ \s -> runParser p s ++ runParser q s

eof :: Parser ()
eof = Parser $ \s -> case s of
    []          -> [((),[])]
    _ : _       -> []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p       = Parser $ \s -> case s of
    []              -> []
    c : cs
        | p c       -> [(c,cs)]
        | otherwise -> []

digit :: Parser Char
digit = satisfy (`elem` "0123456789")

letter :: Parser Char
letter = satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'])

char :: Char -> Parser ()
char c = void $ satisfy (== c)

char' :: Char -> Parser ()
char' c = () <$ satisfy (== c)

digit' :: Parser Natural
digit' = charToNat <$> digit

nzDigit :: Parser Natural
nzDigit = charToNat <$> satisfy (`elem`)

charToNat :: Char -> Natural
charToNat c = fromIntergral (fromEnum c) - 48

natural :: Parser Natural
natural = 
        (0 <$ char '0')
    <|> (f <$> nzDigit <*> many digit')
  where
    f :: Natural -> [Natural] -> Natural
    f = foldl' (\acc c -> 10 * acc + d)

integer = f <$> optional sign <*> natural
  where
    f :: Maybe (Natural -> Integer) -> Natural -> Integer
    f Nothing = 
    f Just g  =

    sign :: Parser (Natural -> Integer)
    sign = 
            (toInteger        <$ char '+')
        <|> 
{- note these functions were implicitly implemented by the Applicative and Alternative typeclasses
many :: Parser -> Parser
many p s = s : (p `combine` many p) s


combine :: Parser (a -> b) -> Parser a -> Parser b -- this is basically the applicative signature for <*>
combine p q = Parser $ \s -> do
    (f, t) <- runParser p s
    (a, u) <- runParser q t
    pure (f a , u)


choose :: Parser -> Parser -> Parser
choose p q s = p s ++ q s
-}