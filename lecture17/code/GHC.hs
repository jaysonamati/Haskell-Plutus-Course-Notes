{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHC where

import GHC.Generics
import Control.Monad.State (StateT (..), MonadState (..), MonadTrans (..))

class Eq' a where 
  eq :: a -> a -> Bool
  default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  eq x y = geq (from x) (from y)

instance Eq' Int where
    eq = (==)

instance Eq' Char where
    eq = (==)

instance Eq' Bool

instance Eq' a => Eq' [a]

instance (Eq' a, Eq' b) => Eq' (a, b)

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Generic, Eq', Ser, Default)

instance Eq' a => Eq' (Maybe a)

class GEq f where
    geq :: f a -> f a -> Bool

instance GEq f => GEq (M1 t i f) where
    geq (M1 x) (M1 y) = geq x y

instance Eq' a => GEq (K1 t a) where
    geq (K1 x) (K1 y) = eq x y

instance GEq U1 where
    geq U1 U1 = True

instance (GEq f, GEq g) => GEq (f :+: g) where
    geq (L1 a) (L1 a') = geq a a'
    geq (R1 b) (R1 b') = geq b b'
    geq _     _      = False

instance (GEq f, GEq g) => GEq (f :*: g) where
    geq (a :*: b) (a' :*: b') = geq a a' && geq b b'


data Bit = One | Zero
    deriving (Show, Eq, Ord)

newtype Parser a = Parser (StateT [Bit] [] a)
    deriving newtype (Functor, Applicative, Monad, MonadState [Bit])

runParser :: Parser a -> [Bit] -> [(a, [Bit])]
runParser (Parser x) = runStateT x

deserialize :: Ser a => [Bit] -> Maybe a
deserialize bs = case runParser deserialize' bs of
    [(a, [])] -> Just a
    _         -> Nothing

class Ser a where
    serialize :: a -> [Bit]
    default serialize :: (Generic a, GSer (Rep a)) => a -> [Bit]
    serialize = gserialize . from

    deserialize' :: Parser a
    default deserialize' :: (Generic a, GSer (Rep a)) => Parser a
    deserialize' = to <$> gdeserialize'

class GSer f where
    gserialize :: f a -> [Bit]
    gdeserialize' :: Parser (f a)

instance Ser a => GSer (K1 t a ) where
    gserialize (K1 x) = serialize x
    gdeserialize' = K1 <$> deserialize'

instance GSer f => GSer (M1 t i f) where
    gserialize (M1 x) = gserialize x
    gdeserialize'     = M1 <$> gdeserialize'
    

instance GSer U1 where
    gserialize U1 = []
    gdeserialize' = pure U1

instance (GSer f, GSer g) => GSer (f :*: g) where
    gserialize (a :*: b) = gserialize a <> gserialize b
    gdeserialize' = (:*:) <$> gdeserialize' <*> gdeserialize'    

instance (GSer f, GSer g) => GSer (f :+: g) where

    gserialize (L1 a) = Zero : gserialize a
    gserialize (R1 b) = One  : gserialize b

    gdeserialize' = do
        bs <- get
        case bs of
            [] -> Parser $ lift []
            (Zero : cs) -> put cs >> L1 <$> gdeserialize'
            (One : cs)  -> put cs >> R1 <$> gdeserialize'

{-
instance Ser U where
    serialize U = []
    deserialize' = pure U

instance (Ser a, Ser b) => Ser (a :*: b) where
    serialize (a :*: b) = serialize a <> serialize b
    deserialize' = (:*:) <$> deserialize' <*> deserialize'

-}

instance Ser a => Ser [a]
instance (Ser a, Ser b) => Ser (a, b)
instance Ser Bool

data Rose a = Fork a [Rose a]
    deriving (Show, Generic, Eq', Ser)

exTree :: Tree [Bit]
exTree = Node
    (Leaf [One, Zero])
    (Node (Leaf []) (Leaf [One]))

exRose :: Rose Bool
exRose = Fork True
    [ Fork True []
    , Fork False [Fork False []]
    , Fork True [Fork True [], Fork False []]]


class Default a where
    def :: a
    default def :: (Generic a, GDefault (Rep a)) => a
    def = to gdef

instance Default Int where
    def = 0

class GDefault f where
    gdef :: f a

instance Default a => GDefault (K1 t a ) where
    gdef = K1 def


instance GDefault f => GDefault (M1 t i f) where
    gdef = M1 gdef
    

instance GDefault U1 where
    gdef = U1

instance (GDefault f, GDefault g) => GDefault (f :*: g) where
    gdef = gdef :*: gdef

instance (GDefault f, GDefault g) => GDefault (f :+: g) where
    gdef = L1 gdef

instance Default ()
instance (Default a, Default b) => Default (a, b)
instance (Default a, Default b, Default c) => Default (a, b, c)

instance Default a => Default [a]

class Enumerate a where
    enumerate :: [a]
    default enumerate :: (Generic a, GEnumerate (Rep a)) => [a]
    enumerate = map to genumerate

class GEnumerate f where
    genumerate :: [f a]

instance Enumerate a => GEnumerate (K1 t a ) where
    genumerate = map K1 enumerate


instance GEnumerate f => GEnumerate (M1 t i f) where
    genumerate = map M1 genumerate
    

instance GEnumerate U1 where
    genumerate = [U1]

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :*: g) where
    genumerate  = [x :*: y | x <- genumerate, y <- genumerate]

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :+: g) where
    genumerate = map L1 genumerate <> map R1 genumerate

instance Enumerate a => Enumerate [a]
instance Enumerate Bool
instance Enumerate ()
instance (Enumerate a, Enumerate b) => Enumerate (a, b)
instance (Enumerate a, Enumerate b) => Enumerate (Either a b)
