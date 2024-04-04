module ScratchPad where

import Prelude hiding (reverse, lookup, (||), (++))
import Data.Maybe

not' :: Bool -> Bool
not' True = False
not' False = True

(||) :: Bool -> Bool -> Bool
(||) x y  = (|||) x y
-- (||) False y = y 

(|||) :: Bool -> Bool -> Bool
(|||) True False  = True 
(|||) True True   = True
(|||) False True  = True
(|||) False False = False

-- (||) = (|||)


addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe _        _        = Nothing

liftMaybe :: (a -> b -> c)
          -> Maybe a
          -> Maybe b
          -> Maybe c
liftMaybe f (Just a) (Just b) = Just (f a b)
liftMaybe _ _        _        = Nothing

(++) :: [a] -> [a] -> [a]
(++) _ ys       = ys
(++) (x: xs) ys = x : xs ++ ys

reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ [x]

-- liftMaybe f x y = f <$> x <*> y


type Table k v = [(k,v)]

empty :: Table k v 
empty = []

insert :: k -> v -> Table k v -> Table k v
insert k v xs = (k, v) : xs

delete :: Eq k => k -> Table k v -> Table k v 
delete  _ [] = []
delete k ((k', v) : kvs)
    | k == k'   = delete k kvs
    | otherwise =  (k', v) : delete k kvs

lookup :: Eq k => k -> Table k v -> Maybe v
lookup _ []      = Nothing
lookup k ((k', v) : kvs)
    | k == k' = Just v 
    | otherwise = lookup k kvs


-- Each data intros a new indirection in the haskell compiler
-- the reason for using newtype is to have a 'implementation' free interface to a type 
-- So an indirection is like a pointer in memory

data Transaction = Transaction 
    { trAmount :: Amount
    , trFrom   :: Account
    , trTo     :: Account
    } deriving (Show, Eq)

type Amount  = Int
type Account = String

-- trAmount :: Transaction -> Amount
-- trAmount (Transaction amount _ _ ) = amount

-- trFrom, trTo :: Transaction -> Account
-- tfFrom (Transaction _ from _ ) = from
-- trTo (Transaction _ from _ )   = from


tx = Transaction
    { trAmount = 100
    , trFrom   = "Lars"
    , trTo     = "Karina"
    }
tx' = tx {trAmount = 200}

type Accounts = Table Account Amount

processTransaction :: Transaction -> Accounts -> Accounts
processTransaction (Transaction amt f t) accounts
    | amt < 0   = accounts
    | otherwise = 
        let
            fOld = fromMaybe 0 (lookup f accounts)
            tOld = fromMaybe 0 (lookup f accounts)
        in
            if fOld >= amt
                then insert f (fOld - amt) (insert t (tOld + amt) accounts)
                else accounts

accs :: Accounts
accs = insert "Karina" 100 (insert "Lars" 250 empty)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

tree :: Tree Int
tree = Node (Leaf 7) (Node (Leaf 4) (Leaf 9))

flatten :: Tree a -> [a]
flatten (Leaf a)   = [a]
flatten (Node l r) = flatten l ++ flatten r 

height :: Tree a -> Int
height (Leaf a)   = 0
height (Node l r) = 1 + max (height l) (height r)


data Expr =
      Lit Int
    | Add Expr Expr
    | Neg Expr
    | IfZero Expr Expr Expr

data Expr' a =
      Lit' a
    | Add' Expr' a Expr' a
    | Neg' Expr' a
    | IfZero' Expr' a Expr' a Expr' a

expr :: Expr
expr = IfZero (Lit 7) (Lit 999) (Add (Neg (Lit 2)) (Lit 5))

eval  :: Expr -> Int
eval (Lit n)   = n
eval (Add x y) = eval x + eval y
eval (Neg x)   = - (eval x)
eval (IfZero x y z)
    | eval x == 0  = eval y
    | otherwise    = eval z 