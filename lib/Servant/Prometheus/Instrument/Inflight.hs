{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Servant.Prometheus.Instrument.Inflight (
  Inflight
) where

import           Control.Exception
import           Data.Monoid                          ((<>))
import           Prometheus
import           Servant.Prometheus.Class             (Endpoint (..))
import           Servant.Prometheus.Instrument.Class  (Instrument (..),
                                                       Lifecycle (..),
                                                       RegisteredInstrument (..))
import           Servant.Prometheus.Instrument.Common (asLabel2, describe)


data Inflight      l = Inflight      Endpoint (Vector Label2 Gauge)

instance RegisteredInstrument Inflight where
  allocate endpoint =
    Inflight <$> pure endpoint <*> register definition

    where
      description = "Number of in flight requests for " <> describe endpoint
      summary     = "in_flight"
      definition  = vector ("path", "method") $
                      gauge (Info summary description)

  initialize (Inflight endpoint gauges) = do
    withLabel gauges label (`setGauge` 0)
    return (Inflight endpoint gauges)

    where
      label = asLabel2 endpoint

instance Instrument (Inflight Initialized) where
  update (Inflight endpoint gauges) application request respond =
    bracket_
      (withLabel gauges label incGauge)
      (withLabel gauges label decGauge)
      (application request respond)

    where
      label = asLabel2 endpoint
