module PolymorphismTypeClasses where

import Prelude hiding (fst)

fst' :: (String, Int) -> String
fst' (x, y) = x

fst'' :: (a, Int) -> a
fst'' (x, y) = x

fst :: (a, b) -> a
fst (a, b) = a

restrictedFst :: (Int, Int) -> Int
restrictedFst = fst 


-- It's better to use the more general one in most cases
-- the more restrictive is more suited for libs when one wants to e
-- the compiler removes all type information
-- 

foo1, foo2, foo3, foo4 :: (Int, Int) -> (Int, Int)


bar1, bar2, bar3, bar4 :: (a, a) -> (a, a)
bar1 (x, y) = (y, x)
bar2 (x, y) = (y, x)
bar3 (x, y) = (y, x)
bar4 (x, y) = (y, x)

bax :: (a, b) -> (b, a)
bax (a, b) = (b, a)


parse :: String -> Either Bool Int 
parse "False" =  Left True
parse "0"     =  Right 0 


class Eq a where 
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    -- default definitions
    x == y = not (x /= y)
    x /= y = not (x == y)
    {-# MINIMAL (==) | (/=) #-} -- This is to give the basic implementations required for a type.
                                -- You can have as many as you would like.

-- In haskell there is only structural equality.

-- Can one use functions instead of classes?

instance Eq Bool where

    x /= y = not (x == y)


instance Eq a => Eq [a] where
    []       == []      = True
    (x : xs) == (y: ys) = x == y && ys == xs
    _        == _       = False

    xs /= ys = not (xs == ys)

-- So a class is like a collection of behaviours

allEqual :: Eq a => [a] -> Bool
allEqual []  = True
allEqual [x] = True
allEqual (x: y : zs) = x == y && allEqual (y : zs)




-- class Eq a => Ord a where
-- If you have an Ord class then compiler can figure out you also have 
-- an Eq class

class Show a where 
    show      :: a -> String
    showsPrec :: Int -> a -> ShowS
    showList  :: [a] -> ShowS
-- showList is an optimization of lists
--- So in the tree example it will use showPrec for the children

-- What'a the difference between deriving and type inference?
-- So the algorithms are similar and the point is to help one reduce the amount of code they write

-- [1.. n] works if the type in the list is an Enum class.

-- We could implement our own numeric types. 
-- The inference goes to the most constrained type ... so there is a relation between type inference and deriving

-- parametric polymorphism has not overhead in contrast to 

-- Can you add type-classes to functions at run time?? 

-- Overloaded functions can also be used to define common functionality in terms of just a small interface? 

-- Overloading can not only occur on function arguments but also on result types.

-- Overloading can be used to use different implementations via the same name
