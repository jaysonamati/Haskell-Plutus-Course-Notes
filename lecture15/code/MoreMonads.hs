{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module MoreMonads where

import           Control.Monad
import           Control.Monad.Identity
import           Data.Functor.Compose
import           Data.Map                  (Map)
import qualified Data.Map               as Map
-- import Data.Kinds.Type


fortyTwo :: Int
fortyTwo = runIdentity $ do
    let x = 6
        y = 7
    pure $ x * y

{-
newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap = liftM

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) = ap

instance Monad (Reader r) where
    return :: a -> Reader r a
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    m >>= k = Reader $ \r -> runReader (k (runReader m r)) r
-}

-- ask :: Reader r r
-- ask = Reader id

-- local :: (r -> r) -> Reader r a -> Reader r a
-- local f m = Reader $ \r -> runReader m (f r)

-- ask' :: r -> r
-- ask' = id

-- local' :: (r -> r) -> (r -> a) -> (r -> a)
-- local' f m r = m $ f r 

data Config = Config
    { cfgSigDigits :: Int
    } deriving Show

round' :: Int -> Double -> Double
round' digits x = (fromIntegral (floor (x * 10 ^ digits + 0.5) :: Integer) :: Double) / 10 ^ digits

myDiv :: Config -> Double -> Double -> Double
myDiv cfg x y = round' (cfgSigDigits cfg) $ x / y

myRecip :: Config -> Double -> Double
myRecip cfg = myDiv cfg 1

recips :: Config -> [Double] -> [Double]
recips cfg = map (myRecip cfg)

type M = Reader Config

roundM :: Double -> M Double
roundM x = do
    cfg <- ask
    pure $ round' (cfgSigDigits cfg) x

myDivM :: Double -> Double -> M Double
myDivM x y = roundM $ x / y

myRecipM :: Double -> M Double
myRecipM = myDivM 1

recipsM :: [Double] -> M [Double]
recipsM = mapM myRecipM

withDoublePrecision :: M a -> M a
withDoublePrecision = local (\cfg -> cfg{cfgSigDigits = 2 * cfgSigDigits cfg})



{-
class MonadReader (m :: * -> * -> *) where
    ask :: m r r
    local :: (r -> r) -> m r a -> m r a

instance MonadReader Reader where
    ask = Reader id
    local f m = Reader $ \r -> runReader m (f r)

instance MonadReader (->) where
    ask = id
    local f m r = m $ f r 
-}


class Monad m => MonadReader r m where
    ask :: m r
    local :: (r -> r) -> m a -> m a


instance MonadReader r (Reader r) where
    ask = ReaderT Identity
    local f m = ReaderT $ \r -> runReaderT m (f r)

instance MonadReader r ((->) r) where
    ask = id
    local f m r = m $ f r 


type Env = Map String Int

type EnvErr = ReaderT Env (Either String) -- newtype EnvErr a = EnvErr {runEnvErr :: Env -> Either String a}

{-
instance Functor EnvErr where
    fmap = liftM

instance Applicative EnvErr where
    pure a = EnvErr $ const $ Right a -- we don't look at env and we don't throw exception
    (<*>) = ap

instance Monad EnvErr where
    return = pure

    m >>= k = EnvErr $ \env -> do
        a <- runEnvErr m env
        runEnvErr (k a) env

instance MonadReader Env EnvErr where
    ask = EnvErr Right
    local f m = EnvErr $ \env -> runEnvErr m $ f env
-}

roundM' :: MonadReader Config m => Double -> m Double
roundM' x = do
    cfg <- ask
    pure $ round' (cfgSigDigits cfg) x

myDivM' :: MonadReader Config m => Double -> Double -> m Double
myDivM' x y = roundM' $ x / y

myRecipM' :: MonadReader Config m => Double -> m Double
myRecipM' = myDivM' 1

recipsM' :: MonadReader Config m => [Double] -> m [Double]
recipsM' = mapM myRecipM'

withDoublePrecision' :: MonadReader Config m => m a -> m a
withDoublePrecision' = local (\cfg -> cfg{cfgSigDigits = 2 * cfgSigDigits cfg})

{-
withDouble :: forall r m a. (Num r, MonadReader r m) => m a -> m a
withDouble = local ((2 :: r) *)
-}

class Monad m => MonadReader' m where
    type EnvType m :: *
    ask' :: m (EnvType m)
    local' :: (EnvType m -> EnvType m) -> m a -> m a


instance MonadReader' (Reader r) where
    type EnvType (Reader r) = r
    ask' = ReaderT Identity
    local' f m = ReaderT $ \r -> runReaderT m (f r)

instance MonadReader' ((->) r) where
    type EnvType ((->) r) = r
    ask' = id
    local' f m r = m $ f r 

instance MonadReader' EnvErr where
    type EnvType EnvErr = Env
    ask' = ReaderT $ \env -> Right env
    local' f m = ReaderT $ \env -> runReaderT m $ f env


{-
withDouble :: forall r m a. (Num r, MonadReader' m) => m a -> m a
withDouble = local' (2 *)
-}

class Monad m => MonadReader'' r m | m -> r where
    ask'' :: m r
    local'' :: (r -> r) -> m a -> m a


instance MonadReader'' r (Reader r) where
    ask'' = ReaderT Identity
    local'' f m = ReaderT $ \r -> runReaderT m (f r)

instance MonadReader'' r ((->) r) where
    ask'' = id
    local'' f m r = m $ f r 

withDouble' :: forall r m a. (Num r, MonadReader'' r m) => m a -> m a
withDouble' = local'' ((2 :: r) *)

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a


newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a} -- the m is the underlying monad, r is the 'state'

instance Monad m => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap = liftM

instance Monad m => Applicative (ReaderT r m) where 
    pure:: a -> ReaderT r m a
    pure a = ReaderT $ \_ -> pure a

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    (<*>) = ap

instance Monad m =>  Monad (ReaderT r m) where 
    return :: a -> ReaderT r m a
    return = pure

    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    m >>= k = ReaderT $ \r -> do -- do notation in m!!
        a <- runReaderT m r
        runReaderT (k a) r

instance Monad m => MonadReader'' r (ReaderT r m) where
    ask'' :: ReaderT r m r
    ask'' = ReaderT $ \r -> pure r

    local'' :: (r -> r) -> ReaderT r m a -> ReaderT r m a
    local'' f m = ReaderT $ \r -> runReaderT m $ f r

type Reader r = ReaderT r Identity

instance MonadTrans (ReaderT r) where
    lift :: Monad m => m a -> ReaderT r m a
    lift m = ReaderT $ const m
    -- lift m = ReaderT $ \_ -> m

runReader :: Reader r a -> r -> a
runReader m r = runIdentity $ runReaderT m r

class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadError e m => MonadError e (ReaderT r m) where
    throwError :: e -> ReaderT r m a
    throwError = lift . throwError

    catchError :: ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
    catchError m h = ReaderT $ \r ->
        catchError (runReaderT m r) $ \e ->
            runReaderT (h e ) r


foo :: ReaderT Int Maybe Double
foo = do
    n <- ask''
    when (n == 0) $
        lift Nothing
    pure $ 1 / fromIntegral n