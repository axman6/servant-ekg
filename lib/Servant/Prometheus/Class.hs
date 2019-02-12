{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Prometheus.Class (
  HasEndpoint(..),
  Endpoint(..)
) where


import           Control.Monad
import           Data.Hashable      (Hashable (..))
import           Data.Monoid        ((<>))
import           Data.Proxy         (Proxy (..))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           GHC.Generics       (Generic)
import           GHC.TypeLits
import           Network.HTTP.Types (Method)
import           Network.Wai        (Request, pathInfo, requestMethod)
import           Servant.API

data Endpoint = Endpoint
  { pathSegments :: [Text]
  , method       :: Method
  } deriving (Eq, Hashable, Show, Generic)


class HasEndpoint a where
  getEndpoint        :: Proxy a -> Request -> Maybe Endpoint
  enumerateEndpoints :: Proxy a -> [Endpoint]


instance HasEndpoint EmptyAPI where
  getEndpoint      _ _ = Nothing
  enumerateEndpoints _ = []

instance (HasEndpoint (a :: *), HasEndpoint (b :: *)) => HasEndpoint (a :<|> b) where
    getEndpoint _ req =
                getEndpoint (Proxy :: Proxy a) req
        `mplus` getEndpoint (Proxy :: Proxy b) req

    enumerateEndpoints _ =
           enumerateEndpoints (Proxy :: Proxy a)
        <> enumerateEndpoints (Proxy :: Proxy b)

instance (KnownSymbol (path :: Symbol), HasEndpoint (sub :: *))
    => HasEndpoint (path :> sub) where
    getEndpoint _ req =
        case pathInfo req of
            p:ps | p == T.pack (symbolVal (Proxy :: Proxy path)) -> do
                Endpoint{..} <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                return (Endpoint (p:pathSegments) method)
            _ -> Nothing

    enumerateEndpoints _ =
        let endpoints               = enumerateEndpoints (Proxy :: Proxy sub)
            currentSegment          = T.pack $ symbolVal (Proxy :: Proxy path)
            qualify Endpoint{..} = Endpoint (currentSegment : pathSegments) method
        in
            map qualify endpoints

instance (KnownSymbol (capture :: Symbol), HasEndpoint (sub :: *))
    => HasEndpoint (Capture' mods capture a :> sub) where
    getEndpoint _ req =
        case pathInfo req of
            _:ps -> do
                Endpoint{..} <- getEndpoint (Proxy :: Proxy sub) req{ pathInfo = ps }
                let p = T.pack $ (':':) $ symbolVal (Proxy :: Proxy capture)
                return (Endpoint (p:pathSegments) method)
            _ -> Nothing
    enumerateEndpoints _ =
        let endpoints               = enumerateEndpoints (Proxy :: Proxy sub)
            currentSegment          = T.pack $ (':':) $ symbolVal (Proxy :: Proxy capture)
            qualify Endpoint{..} = Endpoint (currentSegment : pathSegments) method
        in
            map qualify endpoints

instance HasEndpoint (sub :: *) => HasEndpoint (Summary d :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Description d :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Header' mods h a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParam' mods (h :: Symbol) a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryParams (h :: Symbol) a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (QueryFlag h :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (ReqBody' mods cts a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

#if MIN_VERSION_servant(0,15,0)
instance HasEndpoint (sub :: *) => HasEndpoint (StreamBody' mods framing ct a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)
#endif

instance HasEndpoint (sub :: *) => HasEndpoint (RemoteHost :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (IsSecure :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (HttpVersion :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (Vault :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasEndpoint (sub :: *) => HasEndpoint (WithNamedContext x y sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoint (Verb method status cts a) where
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just (Endpoint [] method)
        _                                -> Nothing
        where method = reflectMethod (Proxy :: Proxy method)

    enumerateEndpoints _ = [Endpoint mempty method]
        where method = reflectMethod (Proxy :: Proxy method)

instance ReflectMethod method => HasEndpoint (Stream method status framing ct a) where
    getEndpoint _ req = case pathInfo req of
        [] | requestMethod req == method -> Just (Endpoint [] method)
        _                                -> Nothing
        where method = reflectMethod (Proxy :: Proxy method)

    enumerateEndpoints _ = [Endpoint mempty method]
        where method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoint Raw where
    getEndpoint      _ _ = Just (Endpoint [] "RAW")
    enumerateEndpoints _ =      [Endpoint [] "RAW"]

instance HasEndpoint (sub :: *) => HasEndpoint (CaptureAll (h :: Symbol) a :> sub) where
    getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
    enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)
