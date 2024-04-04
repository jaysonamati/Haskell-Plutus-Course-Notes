{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}


module FreeMonads.Free
    ( Free (..)
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

data EvalOp b =
      DivByZeroError                            
    | UnknownVar     String                   
    | VarLookup      String (Int -> b)
    | VarSet         String Int b
    deriving (Functor)

data GPOp b = 
      Get (Int -> b)
    | Put Int b
    deriving (Functor)

data Free f a =
      Pure a
    | Free (f (Free f a))

type Eval = Free EvalOp
type GP = Free GPOp

instance Functor f => Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap = liftM

instance Functor f =>  Applicative (Free f) where
    pure :: a -> Free f a
    pure = Pure

    (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    (<*>) = ap

instance Functor f => Monad (Free f) where
    return :: a -> Free f a
    return = pure

    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    Pure x >>= k  = k x
    Free f >>= k  = Free $ (>>= k) <$> f
    -- f :: f (Free f a)
    -- k :: a -> Free f b
    -- (>>= k) :: Free f a -> Free f b
    -- fmap (>>= k) :: f (Free f a) -> f (Free f b)


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
fromEval (Free DivByZeroError)   = MTL.divByZeroError
fromEval (Free (UnknownVar s))   = MTL.unknownVar s
fromEval (Free (VarLookup s k))  = MTL.varLookup s >>= fromEval . k
fromEval (Free (VarSet s n k))   = MTL.varSet s n >> fromEval k



divByZeroError :: Eval a
divByZeroError = Free DivByZeroError

unknownVar :: String -> Eval a
unknownVar s = Free (UnknownVar s)

varLookup :: String -> Eval Int
varLookup s = Free (VarLookup s pure)

varSet :: String -> Int -> Eval ()
varSet s n = Free (VarSet s n $ pure ())



{-
-- With this we loose some information when we were using functions

-- It is also not clear how to implement monads for this.

-- All the data constructors give the same type (Eval a)

-- Using GADTs enables us to use other values of a for instance Eval Int, Eval ()

-- The instances for MOnads become even more interesting.

-}

{-
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
