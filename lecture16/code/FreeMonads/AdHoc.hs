{-# LANGUAGE InstanceSigs #-}

module FreeMonads.AdHoc
    ( Eval (..)
    , eval
    ) where

import           FreeMonads.Expr
import           Control.Monad (liftM, ap)

import qualified FreeMonads.MTL as MTL

{-
data Eval a where
    DivByZeroError ::                              Eval a
    UnknownVar     :: String                    -> Eval a
    VarLookup      :: String -> (Int -> Eval a) -> Eval a
    VarSet         :: String -> Int -> Eval a   -> Eval a
    Pure           :: a                         -> Eval a
-}

data Eval a =
      Pure           a
    | DivByZeroError                            
    | UnknownVar     String                   
    | VarLookup      String (Int -> Eval a)
    | VarSet         String Int (Eval a)                        

-- return x >>= f 
-- Pure x >>= f
-- Bind (Pure x) f
-- f x (not the same !!!)

{-
in MTL.Eval the monad laws hold, so in a sense it doesn't matter if 
    they don't hold in Eval

- after putting the continuations in the type we don't need GADTs anymore

-- After including the continuations in the data constructors the monad laws now follow

-- using the continuations in the constructor allows you to do multiple steps in one computation
-}


fromEval :: Eval a -> MTL.Eval a
fromEval (Pure a)         = pure a
fromEval DivByZeroError   = MTL.divByZeroError
fromEval (UnknownVar s)   = MTL.unknownVar s
fromEval (VarLookup s k)  = MTL.varLookup s >>= fromEval . k
fromEval (VarSet s n k)   = MTL.varSet s n >> fromEval k




divByZeroError :: Eval a
divByZeroError = DivByZeroError

unknownVar :: String -> Eval a
unknownVar = UnknownVar

varLookup :: String -> Eval Int
varLookup s = VarLookup s pure

varSet :: String -> Int -> Eval ()
varSet s n = VarSet s n $ pure ()



{-
-- With this we loose some information when we were using functions

-- It is also not clear how to implement monads for this.

-- All the data constructors give the same type (Eval a)

-- Using GADTs enables us to use other values of a for instance Eval Int, Eval ()

-- The instances for MOnads become even more interesting.

-}

instance Functor Eval where
    fmap :: (a -> b) -> Eval a -> Eval b
    fmap = liftM

instance Applicative Eval where
    pure :: a -> Eval a
    pure = Pure

    (<*>) :: Eval (a -> b) -> Eval a -> Eval b
    (<*>) = ap

instance Monad Eval where
    return :: a -> Eval a
    return = pure

    (>>=) :: Eval a -> (a -> Eval b) -> Eval b
    Pure x         >>= k  = k x
    DivByZeroError >>= _  = DivByZeroError
    UnknownVar s   >>= _  = UnknownVar s
    VarLookup s k  >>= k' = VarLookup s $ \n -> k n >>= k' -- (varLookup s >>= k) >>= k' = varLookup s >>= (k >>= k')
    VarSet s n k   >>= k' = VarSet s n $ k >>= k'

{-
data Tree' a =
      Leaf' a
    | Node' (Tree' a) (Tree' a)

-- with GADTs syntax
data Tree a where
    Leaf :: a -> Tree a
    Node :: Tree a -> Tree a -> Tree a

data List' a =
      Nil'
    | Cons' a (List' a)

-- with GADTs syntax
data List a where
    Nil :: List a
    Cons :: a -> List a -> List a
-}


eval :: Expr -> Eval Int
eval (Lit n)      = pure n
eval (Add x y)    = (+) <$> eval x <*> eval y
eval (Div x y)    = do
    n <- eval y
    if n == 0
        then divByZeroError
        else do
            m <- eval x
            pure $ div m n
eval (Var s)      = varLookup s
eval (Seq x y)    = eval x >> eval y
eval (Assign s x) = do
    n <- eval x
    varSet s n
    pure n

test :: Either String Int
test = MTL.runEval $ fromEval $ eval program