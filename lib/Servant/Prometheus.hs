{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Servant.Prometheus (
    makeMeters,
    monitorServant,
    servePrometheusMetrics,
    MeasureQuantiles(..),
    HasEndpoint(..),
    Endpoint(..),
    Meters,
    metersInflight,
    metersResponses,
    metersTime,
    metersTimeQuant,
    metersRecordQuants
) where

import           Control.Exception
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


gaugeInflight :: Gauge -> Middleware
gaugeInflight inflight application request respond =
    bracket_ (incGauge inflight)
             (decGauge inflight)
             (application request respond)

-- | Count responses with 2XX, 4XX, 5XX, and XXX response codes.
countResponseCodes
    :: Vector Label1 Counter
    -> Middleware
countResponseCodes codes application request respond =
    application request respond'
  where
    respond' res = count (responseStatus res) >> respond res
    count Status{statusCode = sc }
        | 200 <= sc && sc < 300 = withLabel codes "2XX" incCounter
        | 400 <= sc && sc < 500 = withLabel codes "4XX" incCounter
        | 500 <= sc && sc < 600 = withLabel codes "5XX" incCounter
        | otherwise             = withLabel codes "XXX" incCounter

responseTimeDistribution :: MeasureQuantiles -> Histogram -> Maybe Summary -> Middleware
responseTimeDistribution _qants _hist Nothing application request respond = application request respond
responseTimeDistribution qants hist (Just qant) application request respond =
    bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
        t2 <- getCurrentTime
        let dt = diffUTCTime t2 t1
            t = fromRational $ (*1000) $ toRational dt
        observe hist t
        case qants of
            WithQuantiles -> observe qant t
            NoQuantiles   -> pure ()

data Meters = Meters
    { metersInflight     :: Gauge
    , metersResponses    :: Vector Label1 Counter
    , metersTime         :: Histogram
    , metersTimeQuant    :: Maybe Summary
    , metersRecordQuants :: MeasureQuantiles
    }

-- | Measuring quantiles can add significant overhead to your application if your
-- requests are often small. You should benchmark your app with and without
-- quantiles to decide if the overhead is acceptable for you application.
data MeasureQuantiles = WithQuantiles | NoQuantiles deriving (Show, Eq)


makeMeters :: HasEndpoint api => Proxy api -> MeasureQuantiles -> IO (H.HashMap Endpoint Meters)
makeMeters proxy metersRecordQuants = do
    meters <- mapM (makeMeter metersRecordQuants) endpoints

    return $ H.fromList (zip endpoints meters)

    where
        endpoints = enumerateEndpoints proxy

makeMeter :: MeasureQuantiles -> Endpoint -> IO Meters
makeMeter metersRecordQuants Endpoint{..} = do
    metersInflight  <- register mMetersInflight
    metersResponses <- register mMetersResponses
    metersTime      <- register mMetersTime

    metersTimeQuant <- case metersRecordQuants of
        NoQuantiles   -> pure Nothing
        WithQuantiles -> Just <$> register mMetersTimeQant

    return Meters{..}

    where
        path   = T.intercalate "." $ pathSegments <> [T.decodeUtf8 method]
        prefix = "servant.path." <> path <> "."

        mMetersInflight  = gauge $
                                info prefix  "in_flight" "Number of in flight requests for "
        mMetersResponses = vector "status_code" $
                                counter (info prefix "http_status" "Counters for status codes")
        mMetersTime      = histogram
                                (info prefix "time_ms" "Distribution of query times for ")
                                [10,50,100,150,200,300,500,1000,1500,2500,5000,7000,10000,50000]
        mMetersTimeQant  = summary
                                (info prefix "time_ms" "Summary of query times for ")
                                defaultQuantiles

        info :: Text -> Text -> Text -> Info
        info prfx name help = Info (prfx <> name) (help <> prfx)

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
                updateMeters endpointMeters application request respond
    where
        updateMeters Meters{..} =
            responseTimeDistribution metersRecordQuants metersTime metersTimeQuant
                . countResponseCodes metersResponses
                . gaugeInflight metersInflight

-- | An application which will always return prometheus metrics with status 200.
-- This can be added to a Servant API using the RAW type, or may be run in a
-- second webserver on a different port to keep metrics reporting separate from
-- your application.
servePrometheusMetrics :: Application
servePrometheusMetrics _req respond =
    respond . responseLBS status200 [] =<< exportMetricsAsText
