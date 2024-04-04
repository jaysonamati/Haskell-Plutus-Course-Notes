module FreeMonads.Expr where



import Control.Monad.Except (throwError, ExceptT, MonadError, runExceptT)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data Expr = 
      Lit Int
    | Add Expr Expr
    | Div Expr Expr
    | Var String
    | Seq Expr Expr
    | Assign String Expr
    deriving (Show, Eq, Ord)

program :: Expr
program = 
          Assign "x" (Lit 16)
    `Seq` Assign "x" (Div (Var "x") (Lit 2))
    `Seq` Add (Var "x") (Lit 1)

program' :: Expr
program' = 
          Assign "x" (Lit 16)
    `Seq` Assign "x" (Div (Var "x") (Lit 2))
    `Seq` Add (Var "y") (Lit 1)


type Env = Map String Int

-- DSL Domain Specific Language

-- A type has no meaning itself. 

data ExprException = DivisionByZero | UnKnownVariable String
    deriving Show

--                       error-type           
newtype M a = M {runM :: ExceptT ExprException (State Env) a}
    deriving (Functor, Applicative, Monad, MonadError ExprException, MonadState Env)

-- | Given an expr we get an Int in the M Monad, semantic meaning of how it relates to integers
evalM :: Expr -> M Int
evalM (Lit n) = pure n
evalM (Add x y) = (+) <$> evalM x <*> evalM y
evalM (Div x y) = do
    n <- evalM y
    if n == 0
        then throwError DivisionByZero
        else do
            m <- evalM x
            pure $ div m n
evalM (Var v)    = do
    env <- get
    case Map.lookup v env of
        Nothing -> throwError $ UnKnownVariable v
        Just n -> pure n
evalM (Seq x y) = evalM x >> evalM y
evalM (Assign v x) = do
    n <- evalM x
    modify $ Map.insert v n
    pure n

eval :: Expr -> Either ExprException Int
eval expr = runIdentity $ evalStateT (runExceptT $ runM $ evalM expr) mempty

-- "Turing complete" (programming) are languages that can do "everything" for which there is an "algorithm"
-- We wanted something like this but instead of an "algorithm" we have a 'dynamic(or dynamism)'
-- very powerful but also disadvantages i.e "halting problem" : "there can' be a program that inspects another program and says whether that
-- other program will halt/stop/finish or not."
---- such that it's difficult to explore(analyze) them without running it 

-- assume we had something like this:
-- Given an arbitrary IO action, halts will return True if that IO action, when executed will return an Int without throwing an exception
-- or entering an infinite loop, and returns False otherwise.
halts :: IO Int -> Bool
halts = error "can't be implemented"

foo :: IO Int
foo = do
    if halts foo
        then foo
        else pure 42

-- Marlowe is not turing complete because the designers wanted to have static analysis tools.
-- It is less expressive but this gives other powers

canThrowUnknownVariable :: Expr -> Set String
canThrowUnknownVariable  expr = evalState (canThrowUnknownVariable' expr) mempty

canThrowUnknownVariable' :: Expr -> State (Set String) (Set String)
canThrowUnknownVariable' (Lit _)      = pure mempty
canThrowUnknownVariable' (Add x y)    = (<>) <$> canThrowUnknownVariable' x <*> canThrowUnknownVariable' y
canThrowUnknownVariable' (Div x y)    = (<>) <$> canThrowUnknownVariable' x <*> canThrowUnknownVariable' y
canThrowUnknownVariable' (Var v)      = do
    vs <- get
    if Set.member v vs
        then pure mempty
        else pure $ Set.singleton v
canThrowUnknownVariable' (Seq x y)    = (<>) <$> canThrowUnknownVariable' x <*> canThrowUnknownVariable' y
canThrowUnknownVariable' (Assign v x) = do
    vs <- canThrowUnknownVariable' x
    modify $ Set.insert v
    pure vs


-- differences between blockchain and "real-world" contracts:
-- Nobody can force anybody to do anything, like make a payment.
-- A blockchain doesn't "do" anything - every change is triggered by an external event (transaction).

-- Marlowe "accounts":
-- external parties and internal accounts
-- each external party has an internal account
-- initially, all internal accounts are empty
-- on close all funds in internal accounts get paid to corresponding external party