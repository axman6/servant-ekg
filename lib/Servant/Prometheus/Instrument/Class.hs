{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

module Servant.Prometheus.Instrument.Class (
  Lifecycle(..),
  Instrument(..),
  RegisteredInstrument(..)
) where

import           Control.Exception
import           Control.Monad            (forM_)
import           Control.Monad.IO.Class   (MonadIO)
import qualified Data.HashMap.Strict      as H
import           Data.Monoid              ((<>))
import           Data.Proxy
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock
import           Network.HTTP.Types       (Status (..), status200)
import           Network.Wai
import           Prometheus
import           Servant.Prometheus.Class (Endpoint (..), HasEndpoint (..))


data Lifecycle = Allocated | Initialized

class Instrument (a Initialized) => RegisteredInstrument (a :: Lifecycle -> *) where
  allocate   :: (MonadIO m, MonadMonitor m) => Endpoint -> m (a Allocated)
  initialize :: MonadMonitor m => a Allocated -> m (a Initialized)

class Instrument a where
  update :: a -> Middleware
