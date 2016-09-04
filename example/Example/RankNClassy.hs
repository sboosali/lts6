{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module Example.RankNClassy where

import           Control.Category
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Prelude                hiding (id, (.))
import qualified Prelude                as P

newtype Interpret c d = Interpret (forall a. (forall m. c m => m a) -> (forall n. d n => n a))
-- Interpret (forall n a. d n => (forall m. c m => m a) -> n a)
-- Interpret (forall a m n. (c m, d n) => m a -> n a)
-- Interpret (forall a. (forall m. c m => m a) -> (forall n. d n => n a))

instance Category Interpret where
  id = Interpret P.id
  Interpret f . Interpret g = Interpret $ \h -> f (g h)

class Monad m => MonadHttp m where
  httpGet :: String -> m String

newtype HttpApp a = HttpApp { runHttpApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadHttp HttpApp where
  httpGet _ = return "[]" -- Should do actual IO

runIO :: Interpret MonadHttp MonadIO
runIO = Interpret $ \x -> liftIO $ runHttpApp x

newtype MockHttp m a = MockHttp { runMockHttp :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadReader r m => MonadReader r (MockHttp m) where
  ask = MockHttp ask
  local f (MockHttp m) = MockHttp $ local f m

instance MonadReader String m => MonadHttp (MockHttp m) where
  httpGet _ = ask

runMock :: Interpret MonadHttp (MonadReader String)
runMock = Interpret runMockHttp

class Monad m => MonadRestApi m where
  getUserIds :: m [Int]

data RestApi a = GetUsers ([Int] -> a) deriving Functor

instance MonadRestApi (Free RestApi) where
  getUserIds = liftF $ GetUsers id

runRestApi :: Interpret MonadRestApi MonadHttp
runRestApi = Interpret $ iterA go where
  go (GetUsers f) = do
    response <- httpGet "url"
    f $ read response

runApplication :: Interpret MonadRestApi MonadIO
runApplication = runIO . runRestApi

mockApplication :: Interpret MonadRestApi (MonadReader String)
mockApplication = runMock . runRestApi
