module Main (main) where

import     Test.QuickCheck -- turning it to a proper test suite
import     Testing (sort)

main :: IO ()
main = putStrLn "Test suite not yet implemented."


sortPreservesLength :: [Int] -> Bool
sortPreservesLength xs = length (sort xs) == length xs


-- $ cabal test --enable-coverage