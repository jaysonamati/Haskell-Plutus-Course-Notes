module FreeMonads.Expr where



import Control.Monad.Except (throwError)
import Control.Monad.Identity (Identity)
import Data.Map (Map)


data Expr = 
      Lit Int
    | Add Expr Expr
    | Div Expr Expr
    | Var String
    | Seq Expr Expr
    | Assign String Expr
    deriving (Show, Eq, Ord)

type Env = Map String Int

{- here we started using the Identity monad
eval :: Expr -> Identity Int
eval (Lit n)   = pure n
eval (Add x y) = (+) <$> eval x <*> eval y
-}

{-
eval :: Expr -> Int
eval (Lit n)   = n
eval (Add x y) = eval x + eval y
-}

{- [eval]
TO change from the older version to the above new version we 
had to rewrite everything to cater for the new case

Using the Identity monad simplifies implementing cases for new 
data constructors
-}

program :: Expr
program = 
          Assign "x" (Lit 16)
    `Seq` Assign "x" (Div (Var "x") (Lit 2))
    `Seq` Add (Var "x") (Lit 1)