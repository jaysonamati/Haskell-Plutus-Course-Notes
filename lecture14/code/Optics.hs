{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ImpredicativeTypes  #-} -- for the 'nestedLens'
module Optics where


import           Control.Category
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Monoid
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.Map             as Map
import Data.Map                          (Map)
import Prelude  hiding (id, (.))


data Company = Company 
    { _staff  :: [Person]
    , _domain :: String
    } deriving Show

data Person = Person
    { _name :: String
    , _address :: Address
    } deriving Show

data Address = Address 
    { _city    :: String
    , _country :: String
    } deriving Show

karina, lars :: Person
karina = Person
    { _name  = "Karina"
    , _address = Address 
        { _city    = "Zacatecas"
        , _country = "Mexico"
        }
    }
lars = Person
    { _name  = "Lars"
    , _address = Address 
        { _city    = "Regesburg"
        , _country = "Germany"
        }
    }

iog :: Company
iog = Company 
    { _staff = [lars, karina]
    , _domain = "Code"
    }
             

-- | Move all staff of a company to the given city
-- goTo :: String -> Company -> Company
-- goTo city company = 
--     let staff = _staff company
--     in Company $ map (changeCity city) staff
--   where
--     changeCity :: String -> Person -> Person
--     changeCity newCity p = p {_address = Address newCity}

goTo' :: String -> Company -> Company
goTo' city company = company {_staff = map f $ _staff company}
    where
        f :: Person -> Person
        f person = person {_address = g $ _address person}

        g :: Address -> Address
        g address = address {_city = city}

-- data Lens s a = Lens
--     { get :: s -> a
--     , set :: s -> a -> s
--     }

-- data Lens s a = Lens
--     { get   :: s -> a
--     , overF :: forall f. Functor f => (a -> f a) -> (s -> f s)
--     }

type Lens s t a b     = forall f. Functor f => (a -> f b) -> (s -> f t)
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

type Lens' s a        = Lens s s a a
type Traversal' s a  = Traversal s s a a

-- type Lens s t a b     = forall f. Functor f => (a -> f b) -> (s -> f t)
-- type Traversal s a = forall f. Applicative f => (a -> f a) -> (s -> f s)


-- Lens a x      = forall f. Functor f => (a -> f x) -> (a -> f a)
-- Lens s a      = forall f. Functor f =>               (a -> f a) -> (s -> f s)

over :: ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> (s -> t)
over l f = runIdentity . l (Identity . f)

set :: ((a -> Identity b) -> (s -> Identity t)) -> s -> b -> t
set l s a = over l (const a) s

view :: ((a -> Const a b) -> (s -> Const a t)) -> s -> a
view l = getConst . l Const -- using overF at f = Const a

-- collects all the focused elements into a list
toListOf :: ((a -> Const [a] b) -> (s -> Const [a] t)) -> s -> [a]
toListOf l = getConst . l (Const . pure) -- using overF at f = Const a

preview :: ((a -> Const (First a) b) -> (s -> Const (First a) t)) -> s -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just) -- using overF at f = Const a

-- get :: Lens s a -> s -> a
-- get l = getConst . l Const -- using overF at f = Const a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens gt st = \f s -> st s <$> f (gt s)

-- over :: Lens s a -> (a -> a) -> (s -> s)
-- over l f = runIdentity . l (Identity . f)

-- over :: forall f. Functor f => ((a -> f a) -> (s -> f s)) -> (a -> a) -> (s -> s)
-- over l f = runIdentity . l (Identity . f)

-- set :: Lens s a -> s -> a -> s
-- set l s a = over l (const a) s

-- get' :: Lens s a -> s -> a
-- get' l = getConst . overF l Const -- using overF at f = Const a


-- lens :: (s -> a) -> (s -> a -> s) -> Lens s a
-- lens gt st = Lens
--     { get = gt
--     , over = \f s -> st s $ f $ gt s
--     }

-- lens :: (s -> a) -> (s -> a -> s) -> Lens s a
-- lens gt st = Lens
--     { get = gt
--     -- , overF = \f s -> fmap (st s) $ f $ gt s
--     , overF = \f s -> st s <$> f (gt s)
--     }

staff :: Lens' Company [Person]
staff = lens _staff $ \c ps -> c {_staff = ps}

domain :: Lens' Company String
domain = lens _domain $ \c d -> c {_domain = d}

name :: Lens' Person String
name = lens _name $ \p n -> p {_name = n}

address :: Lens' Person Address
address = lens _address $ \p a -> p {_address = a}

city :: Lens' Address String
city = lens _city $ \a c -> a {_city = c}

country :: Lens' Address String
country = lens _country $ \a c -> a {_country = c}


-- _1 :: Lens (a, b) a
-- _1 = lens fst $ \(_, b) a -> (a, b)

_1 :: Lens (a, b) (a', b) a a'
_1 = lens fst $ \(_, b) a -> (a, b)

-- _2 :: Lens (a, b) b
-- _2 = lens snd $ \(a, _) b -> (a, b)

_2 :: Lens (a, b) (a, b') b b'
_2 = lens snd $ \(a, _) b -> (a, b)

lazy :: Lens' BS.ByteString LBS.ByteString
lazy = lens BS.fromStrict $ \_ l -> BS.toStrict l

strict :: Lens' LBS.ByteString BS.ByteString
strict = lens BS.toStrict $ \_ l -> BS.fromStrict l

at :: forall k a. Ord k => k -> Lens' (Map k a) (Maybe a)
at k = lens gt st
  where
    gt :: Map k a -> Maybe a
    gt m = Map.lookup k m 

    st :: Map k a -> Maybe a -> Map k a
    st m (Just a) = Map.insert k a m
    st m Nothing  = Map.delete k m 

-- instance Category Lens where

--     id :: Lens a a
--     -- id = lens id $ \_ a' -> a'
--     id = Lens id ($)
--     -- id - Lens
--     --     { get = id
--     --     , over = ($)
--     --     }

--     (.) :: Lens a x -> Lens s a -> Lens s x
--     ax . sa = Lens
--         { get = get ax . get sa
--         -- , over = \s x -> set sa s $ set ax (get sa s) x
--         , overF = overF sa . overF ax
--         }

-- compose :: Lens a x -> Lens s a -> Lens s x
-- compose ax sa = Lens
--     { get = get ax . get sa
--     , over = \s x -> set sa s $ set ax (get sa s) x
--     }

-- goTo'' :: String -> Company -> Company
-- goTo'' c company = company {_staff = map f $ _staff company}
--     where
--         f :: Person -> Person
--         f p = set (compose city address) p c

-- goTo''' :: String -> Company -> Company
-- goTo''' c company = company {_staff = map f $ _staff company}
--     where
--         f :: Person -> Person
--         f p = set (city . address) p c

goTo'''' :: String -> Company -> Company
goTo'''' c company = company {_staff = map f $ _staff company}
    where
        f :: Person -> Person
        f p = set (address . city) p c

-- over :: Lens s a -> (a -> a) -> (s -> s)
-- over l f s = set l s $ f $ get l s

-- overF :: Functor f => Lens s a -> (a -> f a) -> (s -> f s)
-- overF l f s = fmap (set l s) $ f $ get l s

foo :: String -> IO String
foo s = do
    putStrLn $ "old name: " <> s
    getLine

each :: Traversable t => Traversal (t a) (t b) a b
each = traverse

-- each :: Traversable t => Traversal (t a) a
-- each = traverse

both :: Traversal (a, a) (b, b) a b
both f (a, a') = (,) <$> f a <*> f a'

-- both :: Traversal (a, a) a
-- both f (a, a') = (,) <$> f a <*> f a'

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Functor, Foldable, Traversable)

tree :: Tree String
tree = Node
    (Leaf "Haskell")
    (Node
        (Leaf "Python")
        (Leaf "Java"))

goTo''''' :: String -> Company -> Company
goTo''''' c company = set (staff . each . address . city) company c

-- nestingLens :: Lens (Lens (a, b) a) (Lens (a, b) b)
-- nestingLens = _2 . _1