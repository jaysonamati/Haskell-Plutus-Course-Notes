{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}

module MoreOptics(ex) where

import Control.Monad
import Data.Profunctor
import Data.Tagged
import Data.Functor.Identity
import Data.Monoid
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.Text            as T
import           Data.Text               (Text (..))

import Optics


type Prism s t a b = forall f p. (Applicative f, Choice p) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a 


-- type Prism s a = forall f p. (Applicative f, Choice p) => p a (f a) -> p s (f s)


-- data Prism s a = Prism
--     { preview :: s -> Maybe a
--     , review  :: a -> s 
--     }

prism :: forall s t a b. (s -> Either t a) -> (b -> t) -> Prism s t a b
prism p r x = dimap p (either pure $ fmap r) $ right' x 


prism' :: (s -> Maybe a) -> (a -> s) -> Prism' s a
prism' p = prism p'
    where 
        p' s = case p s of
                    Nothing -> Left s
                    Just a  -> Right a

-- prism :: forall s t a b. (s -> Maybe a) -> (b -> t) -> Prism' s a
-- prism p r x = dimap p' (either pure $ fmap r) $ right' x 
--     where 
--         p' :: s -> Either s a
--         p' s = case p s of
--                     Nothing -> Left s
--                     Just a  -> Right a

-- prism :: (s -> Maybe a) -> (a -> s) -> Prism s a
-- prism = Prism

-- idea : use f = Identity ; p = Tagged
-- Tagged a (Identity a) -> Tagged s (Identity s)
-- Identity a -> Identity s
-- a -> s
review :: (Tagged a (Identity b) -> Tagged s (Identity t))-> b -> t
review p = runIdentity . unTagged . p . Tagged . Identity

-- review :: Prism s a -> a -> s
-- review p = runIdentity . unTagged . p . Tagged . Identity

data Result a = Ok a | Error String
    deriving Show

_Ok :: Prism (Result a) (Result b) a b
_Ok = prism p r
    where 
        p :: Result a -> Either (Result b) a
        p (Ok a)    = Right a
        p (Error e) = Left (Error e)

        r :: a -> Result a
        r = Ok 

-- _Ok :: Prism (Result a) (Result b) a b
-- _Ok = prism p r
--     where 
--         p :: Result a -> Maybe a
--         p (Ok a)    = Just a
--         p (Error _) = Nothing

--         r :: a -> Result a
--         r = Ok 


_Error :: Prism' (Result a) String
_Error = prism' p r
    where 
        p :: Result a -> Maybe String
        p (Ok _)    = Nothing
        p (Error e) = Just e

        r :: String -> Result a
        r = Error

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism p Just
    where
        p :: Maybe a -> Either (Maybe b) a
        p (Just a) = Right a
        p Nothing  = Left Nothing

-- _Just :: Prism (Maybe a) (Maybe b) a b
-- _Just = prism id Just

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' p r
    where 
        p :: Maybe a -> Maybe ()
        p (Just _)  = Nothing
        p (Nothing) = Just ()

        r :: () -> Maybe a
        r () = Nothing

_Left :: Prism (Either a b) (Either a' b) a a'
_Left = prism p Left
    where
        p :: Either a b -> Either (Either a' b) a
        p (Left a) = Right a
        p (Right b) = Left (Right b)

-- _Left :: Prism (Either a b) (Either a' b) a' b
-- _Left = prism (either Just $ const Nothing) Left

_Right :: Prism (Either a b) (Either a b') b b'
_Right = prism p Right
    where
        p :: Either a b -> Either (Either a b') b
        p (Right b) = Right b
        p (Left a)  = Left (Left a)

-- _Right :: Prism (Either a b) (Either a b') a b'
-- _Right = prism (either (const Nothing) Just) Right

_Nil :: Prism' [a] ()
_Nil = prism' p r
    where
        p :: [a] -> Maybe ()
        p []     = Just ()
        p (_ : _) = Nothing

        r :: () -> [a]
        r () = []


_Cons :: Prism [a] [b] (a, [a]) (b, [b])
_Cons = prism p r
    where
        p :: [a] -> Either [b] (a, [a])
        p []     = Left []
        p (x : xs) = Right (x, xs)

        r :: (a, [a]) -> [a]
        r (x, xs) = x : xs

_Id :: Iso a b a b 
_Id = iso id id

-- _Id :: Prism a b a b 
-- _Id = prism Just id

-- instance Category Prism where
--     id :: Prism a a
--     id = _Id

--     (.) :: Prism a x -> Prism s a -> Prism s x
--     ax . sa = prism p r
--         where
--             p :: s -> Maybe x
--             p = preview sa `>=>` preview ax
--             -- p s = do
--             --     a <- preview sa a
--             --     preview ax a

--             r :: x -> s
--             r = review sa . review ax

ex :: (Bool, [Maybe (Either Bool String)])
ex = (False, [Nothing, Just (Left True), Nothing, Just (Right "Haskell"), Just (Left False), Nothing, Just (Right "Kenya")])


type Iso s t a b = forall f p. (Functor f, Choice p) => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a 


iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso v r = dimap v $ fmap r

re :: Iso s t a b -> Iso b a t s
re i = iso (review i) (view i)


maybeToEither :: forall a b. Iso (Maybe a) (Maybe b) (Either () a) (Either () b)
maybeToEither = iso v r
    where
        v :: Maybe a -> Either () a
        v Nothing  = Left ()
        v (Just a) = Right a

        r :: Either () b -> Maybe b
        r (Left ()) = Nothing
        r (Right b) = Just b

reversed :: Iso [a] [b] [a] [b]
reversed = iso reverse reverse

-- reversed :: Iso' [a] [a]
-- reversed = iso reverse reverse

curried :: Iso ((a,b) -> c) ((a',b') -> c') (a -> b -> c) (a' -> b' -> c')
curried = iso curry uncurry

-- curried :: Iso' ((a,b) -> c) (a -> b -> c)
-- curried = iso curry uncurry

flipped :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = iso flip flip

-- flipped :: Iso' (a -> b -> c) (b -> a -> c)
-- flipped = iso flip flip

swapped :: Iso (a, b) (a', b') (b, a) (b', a')
swapped = iso swap swap
    where
        swap (a, b) = (b, a)

-- swapped :: Iso' (a, b) (b, a)
-- swapped = iso swap swap
--     where
--         swap (a, b) = (b, a)

swapped' :: Iso (Either a b) (Either a' b') (Either b a) (Either b' a')
swapped' = iso swap' swap'
    where
        swap' (Left a)  = Right a
        swap' (Right b) = Left b

-- swapped' :: Iso' (Either a b) (Either b a)
-- swapped' = iso swap' swap'
--     where
--         swap' (Left a)  = Right a
--         swap' (Right b) = Left b

summed :: Iso a b (Sum a) (Sum b)
summed = iso Sum getSum

-- >> over (each . re summed) (+ 3) [Sum 1, Sum 4]

lazy' :: Iso' BS.ByteString LBS.ByteString
lazy' = iso BS.fromStrict BS.toStrict

strict' :: Iso' LBS.ByteString BS.ByteString
strict' = re lazy'

packed :: Iso' String Text
packed = iso T.pack T.unpack

unpacked :: Iso' Text String
unpacked = re packed

-- >>over (_2 . each . _Just . _Right) ((^ 2) . length) ex