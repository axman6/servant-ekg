{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Servant.Prometheus.Instrument.Common (
  asLabel2,
  describe,
  timeAction
) where

import           Control.Exception        (bracket)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Prometheus
import           Servant.Prometheus.Class (Endpoint (..))

import           Data.Time.Clock          (NominalDiffTime, diffUTCTime,
                                           getCurrentTime)

asLabel2 :: Endpoint -> Label2
asLabel2 Endpoint{..} = (path, method')
    where
        method' = T.decodeUtf8 method
        path    = "/" <> T.intercalate "/" pathSegments

describe :: Endpoint -> Text
describe ep = snd (asLabel2 ep) <> " " <> fst (asLabel2 ep)

timeAction :: (Double -> IO ()) -> IO a -> IO a
timeAction onComplete =
  bracket getCurrentTime onComplete' . const

  where
    onComplete' startTime = do
      stopTime <- getCurrentTime

      let dt = diffUTCTime stopTime startTime
          t  = fromRational $ (*1000) $ toRational dt

      onComplete t
