{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Servant.Prometheus.Metering (
    makeMeter,
    MeasureQuantiles(..),
    Meters,
    update,
) where

import           Servant.Prometheus.Class                    (Endpoint (..))
import           Servant.Prometheus.Instrument.Class         (Instrument (..),
                                                              Lifecycle (..),
                                                              RegisteredInstrument (..))
import           Servant.Prometheus.Instrument.Inflight      (Inflight)
import           Servant.Prometheus.Instrument.ResponseCodes (ResponseCodes)
import           Servant.Prometheus.Instrument.TimeHistogram (TimeHistogram)
import           Servant.Prometheus.Instrument.TimeQuantiles (TimeQuantiles)


data WithQuantiles' = WithQuantiles'
  { wqMetersInFlight  :: Inflight      'Initialized
  , wqMetersResponses :: ResponseCodes 'Initialized
  , wqMetersTime      :: TimeHistogram 'Initialized
  , wqMetersTimeQuant :: TimeQuantiles 'Initialized
  }

instance Instrument WithQuantiles' where
  update WithQuantiles'{..} =
      update wqMetersTimeQuant
    . update wqMetersTime
    . update wqMetersResponses
    . update wqMetersInFlight


data NoQuantiles' = NoQuantiles'
  { nqMetersInFlight  :: Inflight      'Initialized
  , nqMetersResponses :: ResponseCodes 'Initialized
  , nqMetersTime      :: TimeHistogram 'Initialized
  }

instance Instrument NoQuantiles' where
  update NoQuantiles'{..} =
      update nqMetersTime
    . update nqMetersResponses
    . update nqMetersInFlight


data Meters where
  Meters :: Instrument i => i -> Meters

instance Instrument Meters where
  update (Meters instrument) = update instrument


-- | Measuring quantiles can add significant overhead to your application if your
-- requests are often small. You should benchmark your app with and without
-- quantiles to decide if the overhead is acceptable for you application.
data MeasureQuantiles = WithQuantiles | NoQuantiles deriving (Show, Eq)

makeMeter :: MeasureQuantiles -> Endpoint -> IO Meters
makeMeter WithQuantiles ep =
  (((Meters .) .) .) . WithQuantiles'
    <$> (initialize =<< allocate ep)
    <*> (initialize =<< allocate ep)
    <*> (initialize =<< allocate ep)
    <*> (initialize =<< allocate ep)
makeMeter NoQuantiles ep =
  ((Meters .) .) . NoQuantiles'
  <$> (initialize =<< allocate ep)
  <*> (initialize =<< allocate ep)
  <*> (initialize =<< allocate ep)
