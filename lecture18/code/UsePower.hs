{-# LANGUAGE TemplateHaskell #-}
module UsePower where



import Power
import Numeric.Natural (Natural)
import Control.Lens.TH


exp8 :: Int
exp8 = $$exp2

spower10 :: Integer -> Integer
spower10 x = $$(spower 10 [|| x ||])

spower10' :: Integer -> Integer
spower10' = $$(to $ spower 10)


memoFib :: Natural -> Natural
memoFib = $$(smemo [0 .. 5] fib [|| fib ||])

properFib :: Natural -> Natural
properFib n = fibs !! fromIntegral n

fibs :: [Natural]
fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)

-- useTe = $te
{-
$(myFst 4)
$(myFst 5)
$(myFst 6)
$(myFst 7)
$(myFst 8)
-}

$(myFsts [4 .. 20])

fibsInfo :: String
fibsInfo = $(showNameInfo 'fibs)

treeInfo :: String
treeInfo = $(showNameInfo ''Tree)

data Person = Person
    { _name    :: String
    , _age     :: Int
    , _address :: Address
    }

data Address = Address
    { _country  :: String
    , _city     :: String
    }

makeLenses ''Person
makeLenses ''Address
