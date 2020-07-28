{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TupleSections     #-}

module Servant.Prometheus.Instrument.ResponseCodes (
  ResponseCodes
) where

import           Control.Exception
import           Control.Monad                        (forM_)
import           Data.Monoid                          ((<>))
import           Network.HTTP.Types                   (Status (..), status200)
import           Network.Wai                          (responseStatus)
import           Prometheus
import           Servant.Prometheus.Class             (Endpoint (..))
import           Servant.Prometheus.Instrument.Class  (Instrument (..),
                                                       Lifecycle (..),
                                                       RegisteredInstrument (..))
import           Servant.Prometheus.Instrument.Common (asLabel2, describe)


data ResponseCodes l = ResponseCodes Endpoint (Vector Label3 Counter)

-- | Count responses with 2XX, 4XX, 5XX, and XXX response codes.
instance RegisteredInstrument ResponseCodes where
  allocate endpoint =
    ResponseCodes <$> pure endpoint <*> register definition

    where
      description = "Counters for status codes for " <> describe endpoint
      summary     = "http_status"
      definition  = vector ("path", "method", "status_code") $
                      counter (Info summary description)

  initialize (ResponseCodes endpoint codes) = do
    forM_ ["2XX", "4XX", "5XX", "XXX"] $ \code ->
      withLabel codes (label code) (`unsafeAddCounter` 0)

    return (ResponseCodes endpoint codes)

    where
      label          = (path, method,)
      (path, method) = asLabel2 endpoint

instance Instrument (ResponseCodes 'Initialized) where
  update (ResponseCodes endpoint codes) application request respond =
    application request respond'

    where
      respond' res = count (responseStatus res) >> respond res
      count Status{statusCode = sc }
          | 200 <= sc && sc < 300 = withLabel codes (label "2XX") incCounter
          | 400 <= sc && sc < 500 = withLabel codes (label "4XX") incCounter
          | 500 <= sc && sc < 600 = withLabel codes (label "5XX") incCounter
          | otherwise             = withLabel codes (label "XXX") incCounter
      label          = (path, method,)
      (path, method) = asLabel2 endpoint
