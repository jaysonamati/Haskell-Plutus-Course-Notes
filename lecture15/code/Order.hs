{-# LANGUAGE FlexibleContexts #-}
module Order where
    
import Control.Monad.State (MonadState (..), StateT (..), State, runState)
import Control.Monad.Except (MonadError (..), ExceptT (..))
import Control.Monad (when)
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.Reader (MonadTrans(..))

bar :: (MonadError String m, MonadState Int m) => m Double
bar = do
    n <- get
    put $ n + 1
    when (n == 0) $
        throwError "division by zero"
    pure $ 1 / fromIntegral n

baz :: (MonadError String m, MonadState Int m) => m (Double, Double)
baz = (,) <$> bar <*> bar

type M1 = StateT Int (Either String)
type M2 = ExceptT String (State Int)

-- runM1 :: M1 a -> Int -> 
runM1 :: M1 a -> Int -> Either String (a, Int)
runM1 = runStateT

runM2 :: M2 a -> Int -> (Either String a, Int)
runM2 (ExceptT m) = runState m

type M = ExceptT String (ReaderT Int [])

nairobi :: M Double
nairobi = do
    n <- ask
    when (n == 0) $
        throwError "division by zero"
    let d = fromIntegral n
    lift $ lift [1 / d, d + 1]


runM :: M a -> Int -> [Either String a]
runM (ExceptT m) = runReaderT m