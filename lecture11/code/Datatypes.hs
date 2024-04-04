{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Datatypes where

import Data.Kind (Type)

type WrappedInt :: (Type -> Type) -> Type
data WrappedInt (f :: * -> *) = Wrap (f Int :: *)
-- Type is equivalent to * 

ex1 :: WrappedInt Maybe
ex1 = Wrap (Just 42)

ex2 :: WrappedInt []
ex2 = Wrap [1, 2, 5]

ex3 :: WrappedInt IO
ex3 = Wrap readLn

swap :: (a, b) -> (b, a)
swap (n, b) = (b, n)

fmap' :: (a -> b) -> (a, t) -> (b, t)
-- fmap' f (a, t) = (f a, t)
fmap' f = swap . fmap f . swap

type Flip f a b = f b a

ex4 :: Flip (,) Int Bool
ex4 = (True, 42)

ex5 :: [Char]
ex5 = 'a' : ex5


data Tree a = Leaf a | Node a (Tree a) (Tree a)
-- A binary tree is called 'perfect' if all its leaves have the same distance to the root

-- Can we define a datatype of perfect binary trees??
-- an example of nested datatypes
data Perfect a = Zero a | Succ (Perfect (a, a))
    deriving (Show)

tree1, tree2, tree3, tree4 :: Perfect Int
tree1 = Zero 1
tree2 = Succ (Zero (1, 2))
tree3 = Succ (Succ (Zero ((1,2), (3,4))))
tree4 = Succ (Succ (Succ (Zero (((1,2), (3, 4)),((5, 6),(7, 8))))))


sumPerfect :: Perfect Int -> Int
sumPerfect = sumPerfect' id 

sumPerfect' :: forall a. (a -> Int) -> Perfect a -> Int
sumPerfect' f (Zero a) = f a
sumPerfect' f (Succ t) = sumPerfect' g t
    where
        g :: (a, a) -> Int
        g (x, y) = f x + f y 


mySum :: [Int] -> Int
mySum []     = 0
mySum (x:xs) = x + mySum xs

myTailRecursiveSum :: [Int] -> Int
myTailRecursiveSum = myTailRecursiveSum' id

myTailRecursiveSum' :: (Int -> Int) -> [Int] -> Int
myTailRecursiveSum' k []            = k 0
myTailRecursiveSum' k (x : xs) = myTailRecursiveSum' (\s -> k $ x + s) xs

