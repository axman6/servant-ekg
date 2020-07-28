{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Servant.Prometheus.Instrument.TimeQuantiles (
  TimeQuantiles
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

data TimeQuantiles l = TimeQuantiles Endpoint (Vector Label2 Summary)

instance RegisteredInstrument TimeQuantiles where
  allocate endpoint =
    TimeQuantiles <$> pure endpoint <*> register definition

    where
      description = "Summary of query times for " <> describe endpoint
      summary'    = "time_ms"
      definition  = vector ("path", "method") $
                      summary
                        (Info summary' description)
                        defaultQuantiles

  initialize (TimeQuantiles endpoint quantiles) = do
    withLabel quantiles label (`observe` 0)
    return (TimeQuantiles endpoint quantiles)

    where
      label = asLabel2 endpoint

instance Instrument (TimeQuantiles 'Initialized) where
  update (TimeQuantiles endpoint quantiles) application request respond =
    timeAction (withLabel quantiles label . flip observe) (application request respond)

    where
      label = asLabel2 endpoint
