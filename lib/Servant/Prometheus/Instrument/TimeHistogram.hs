{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Servant.Prometheus.Instrument.TimeHistogram (
  TimeHistogram
) where

import           Control.Exception
import           Control.Monad                        (forM_)
import           Data.Monoid                          ((<>))
import           Data.Time.Clock                      (diffUTCTime,
                                                       getCurrentTime)
import           Network.HTTP.Types                   (Status (..), status200)
import           Network.Wai                          (responseStatus)
import           Prometheus
import           Servant.Prometheus.Class             (Endpoint (..))
import           Servant.Prometheus.Instrument.Class  (Instrument (..),
                                                       Lifecycle (..),
                                                       RegisteredInstrument (..))
import           Servant.Prometheus.Instrument.Common (asLabel2, describe,
                                                       timeAction)

data TimeHistogram l = TimeHistogram Endpoint (Vector Label2 Histogram)

instance RegisteredInstrument TimeHistogram where
  allocate endpoint =
    TimeHistogram <$> pure endpoint <*> register definition

    where
      description = "Distribution of query times for " <> describe endpoint
      summary     = "time_ms"
      definition  = vector ("path", "method") $
                      histogram
                        (Info summary description)
                        [10,50,100,150,200,300,500,1000,1500,2500,5000,7000,10000,50000]

  initialize (TimeHistogram endpoint histogram) = do
    withLabel histogram label (`observe` 0)
    return (TimeHistogram endpoint histogram)

    where
      label = asLabel2 endpoint

instance Instrument (TimeHistogram 'Initialized) where
  update (TimeHistogram endpoint histogram) application request respond =
    timeAction (withLabel histogram label . flip observe) (application request respond)

    where
      label = asLabel2 endpoint
