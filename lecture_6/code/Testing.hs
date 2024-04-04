module Testing(sort) where 

import Test.QuickCheck

sort :: [Int] -> [Int]
sort []      = []
sort (x: xs) = insert x $ sort xs

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : xs)
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys

sortPreservesLength :: [Int] -> Bool
sortPreservesLength xs = length (sort xs) == length xs
sortPreservesLength = sort `preserves` length

preserves :: Eq a => (t -> t) -> (t -> a) -> t -> Bool
(f `preserves` p) xs = p (f xs) == p xs

sorted :: [Int] -> Bool
sorted []          = True
sorted [_]         = True 
sorted (x: y : zs) = x <= y && sorted (y : zs)

ensures :: (t -> a) -> (a -> b) -> t -> b
(f `ensures` p) xs = p $ f xs

sortEnsuresSorted :: [Int] -> Bool
sortEnsuresSorted xs = sorted $ sort xs

evilNoSort :: [Int] -> [Int]
evilNoSort xs = replicate (length xs) 42

permutes :: Eq a => ([a] -> [a]) -> [a] -> Bool
f `permutes` p = f xs `elem` permutations xs


appendLength :: [a] -> [a] -> Bool
appendLength xs ys = length (xs ++ ys) == length xs + length ys

plusIsCommutative :: Int -> Int -> Bool
plusIsCommutative x y = x + y == y + x

takeDrop :: Int -> [Int] -> Bool
takeDrop n xs = take n xs ++ drop n xs == xs

dropTwice :: Int -> Int -> [Int] -> Bool
dropTwice m n xs = drop m (drop n xs) == drop (m + n) xs

lengthEmpty :: Bool
lengthEmpty = length [] == 0

implies :: Bool -> Bool -> Bool
implies x y = not x || y

insertPreservesOrdered :: Int -> [Int] -> Bool
insertPreservesOrdered x xs = isSorted xs  `implies` isSorted (insert x xs) -- the second case is trivially true if the generated list is not sorted.

insertPreservesOrdered' :: Int -> [Int] -> Property
insertPreservesOrdered' x xs = isSorted xs  ==> isSorted (insert x xs)


-- Custom generators
-- >> sample (choose (1 :: Int, 6))
-- >> sample (choose (0 :: Double, 1))
-- >> sample $ oneof [choose (0 :: Int, 1), choose (99, 101)] 
-- >> sample $ frequency [(1, choose (0 :: Int, 1)), (3, choose (99, 101))] 
-- >> sample $ elements "cdf"
-- >> sample $ sized $ \n -> if n <= 5 then elements "a" else elements "XYZ"


data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)


-- a random tree of size n should have at most n + 1 leaves

-- generate a random tree with at most the given number of leaves
genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree n 
    | n <= 1    = Leaf <$> arbitrary
    | otherwise = liftM2 Node (genTree m) (genTree m)
        where
            m = n / 2

-- to make more varied random trees we could use frequency or oneof            

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized $ \n -> genTree (n + 1)


instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized $ \n -> do
        m <- elements [1 .. n + 1]
        genTree m    

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = oneof [genLeaf, genNode]
        where
            genLeaf = Leaf <$> arbitrary
            genNode = liftM2 Node arbitrary arbitrary

-- fmap :: (a -> b) -> Gen a -> Gen b
-- fmap (Leaf :: a -> Tree a) = Gen a -> Gen (Tree a)

-- fmap Leaf arbitrary :: Gen (Tree a)

--  a generator for only sorted lists
mkSorted :: [Int] -> [Int]
mkSorted []           = []
mkSorted [x]          = [x]
mkSorted (x : y : ys) = x : mkSorted (x + abs y : ys)

-- How would we use the above
insertPreservesOrdered' :: Int -> [Int] -> Bool
insertPreservesOrdered' x xs = 
    isSorted (insert x ys)
  where
    ys = mkSorted xs

-- Another generator for ints

genSorted :: Gen [Int]
genSorted = mkSorted <$> arbitrary

-- >> sample (genSorted :: Gen [Int])


-- How to use this for quickCheck

insertPreservesOrdered'' :: _  -- you can put holes in fn definitions
insertPreservesOrdered'' x = forAll genSorted $ \xs -> 
    isSorted (insert x xs)

newtype SortedIntList = SortedIntList [Int]
    deriving (Show)

instance Arbitrary SortedIntList where
    arbitrary = SortedIntList <$> genSorted

insertPreservesOrdered''' :: Int -> SortedIntList -> Bool -- This is the nicest one!
insertPreservesOrdered''' x (SortedIntList xs) = 
    isSorted (insert x xs)

insertPreservesOrdered'''' :: Int -> SortedIntList -> Bool -- This is the nicest one!
insertPreservesOrdered'''' x (Ordered xs) = 
    isSorted (insert x xs)    

-- >> sample (arbitrary :: Gen (OrderedList Int))    
-- >> sample (arbitrary :: Gen (NonEmptyList Int))

-- Can you generate functions??

myMap :: (a -> b) -> [a] -> [b]
myMap f []       = []
myMap f (X : xs) = f x : f x : myMap f xs