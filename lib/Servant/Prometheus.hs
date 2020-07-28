{-# LANGUAGE NamedFieldPuns #-}

module Servant.Prometheus (
    makeMeters,
    monitorServant,
    servePrometheusMetrics,
    MeasureQuantiles(..),
    HasEndpoint(..),
    Endpoint(..),
    Meters
) where

import qualified Data.HashMap.Strict         as H
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time.Clock
import           Network.HTTP.Types          (Status (..), status200)
import           Network.Wai
import           Prometheus
import           Servant.Prometheus.Class    (Endpoint (..), HasEndpoint (..))
import           Servant.Prometheus.Metering (MeasureQuantiles (..),
                                              Meters (..), makeMeter, update)


makeMeters :: HasEndpoint api => Proxy api -> MeasureQuantiles -> IO (H.HashMap Endpoint Meters)
makeMeters proxy metersRecordQuants = do
    meters <- mapM (makeMeter metersRecordQuants) endpoints

    return $ H.fromList (zip endpoints meters)

    where
        endpoints = enumerateEndpoints proxy

monitorServant
    :: HasEndpoint api
    => Proxy api
    -> H.HashMap Endpoint Meters
    -> Middleware
monitorServant proxy meters application request respond =
    case getEndpoint proxy request >>= \ep -> H.lookup ep meters of
            Nothing ->
                application request respond
            Just endpointMeters ->
                update endpointMeters application request respond

-- | An application which will always return prometheus metrics with status 200.
-- This can be added to a Servant API using the RAW type, or may be run in a
-- second webserver on a different port to keep metrics reporting separate from
-- your application.
servePrometheusMetrics :: Application
servePrometheusMetrics _req respond =
    respond . responseLBS status200 [] =<< exportMetricsAsText
