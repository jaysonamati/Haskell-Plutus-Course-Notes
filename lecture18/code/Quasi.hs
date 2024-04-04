{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Quasi (ternary) where

import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Quote


fortyTwo10, fortyTwo16, fortyTwo8, fortyTwo2 :: Int
fortyTwo10 = 42
fortyTwo16 = 0x2a
fortyTwo8  = 0o52
fortyTwo2  = 0101010


ternary :: QuasiQuoter
ternary = QuasiQuoter
    { quoteExp  = \s -> let n = parseTernary s in [| n |]
    , quotePat  = \s -> let n = parseTernary s in pure $ LitP $ IntegerL n
    , quoteType = error ""
    , quoteDec  = error ""
    }

parseTernary :: String -> Integer
parseTernary = foldl' f 0
    where
        f :: Integer -> Char -> Integer
        f acc '0' = 3 * acc
        f acc '1' = 3 * acc + 1 
        f acc '2' = 3 * acc + 2
        f _ c     = error ""