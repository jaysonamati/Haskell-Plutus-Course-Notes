{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FreeMonads.MTL
    ( Eval
    , divByZeroError
    , runEval
    , unknownVar
    , varLookup
    , varSet
    , eval
    ) where
import Control.Monad (liftM, ap)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import qualified Data.Map as Map
-- import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Control.Monad.State (StateT (..), MonadState (..), evalStateT, modify)

import FreeMonads.Expr

newtype Eval a = Eval (StateT Env (ExceptT String Identity ) a)
    deriving (Functor, Applicative, Monad, MonadError String, MonadState Env)

{- [Eval a]

This is in a sense an abstract monad.

Using the extension GeneralizednewtypeDeriving we are able to 
use existing derivations of constructors on the right hand side to
'add' those derivations to the type we are creating

In the above since ExceptT / Identity has Functor .. etc

In order to change the environment we change the ReaderT to StateT

The associated functions also change in line with the above.
-}

{-
instance Functor Eval where
    fmap = liftM

instance Applicative Eval where
    pure = undefined

    (<*>) = ap

instance Monad Eval where
    return = pure
    (>>=) = undefined 
-}


divByZeroError :: Eval a
divByZeroError = throwError "division by 0"

unknownVar :: String -> Eval a
unknownVar s = throwError $ "Unknown variable: " <> s

varLookup :: String -> Eval Int
varLookup s = do
    env <- get
    case Map.lookup s env of
        Just n -> pure n
        Nothing -> unknownVar s

varSet :: String -> Int -> Eval ()
varSet s n = modify $ Map.insert s n

runEval :: Eval a -> Either String a
runEval (Eval m) = runIdentity $ runExceptT $ evalStateT m mempty


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