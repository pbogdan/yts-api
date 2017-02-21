{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Yts.Api.Response
  ( PayloadKeyPrefix
  , Payload(..)
  , Meta(..)
  , metaApiVersion
  , metaExecutionTime
  , metaServerTimezone
  , metaServerTime
  , Status(..)
  , Response(..)
  , responseStatus
  , responseMeta
  , responseStatusMessage
  ) where

import Protolude hiding (Meta)

import Control.Lens hiding ((.=))
import Data.Aeson hiding (Error)
import GHC.TypeLits

type family PayloadKeyPrefix (a :: *) :: Symbol
type instance PayloadKeyPrefix [a] = PayloadKeyPrefix a

data Payload (s :: Symbol) a where
        Payload :: a -> Payload (PayloadKeyPrefix a) a

deriving instance Show a => Show (Payload s a)
deriving instance Eq a => Eq (Payload s a)

instance (s ~ PayloadKeyPrefix a, KnownSymbol s, FromJSON a) =>
         FromJSON (Payload s a) where
  parseJSON (Object v) =
    let prefix = toS (symbolVal (Proxy :: Proxy s))
    in Payload <$> (parseJSON =<< v .: prefix)
  parseJSON _ = mzero

instance (s ~ PayloadKeyPrefix a, KnownSymbol s, ToJSON a) =>
         ToJSON (Payload s a) where
  toJSON (Payload x) =
    let prefix = toS (symbolVal (Proxy :: Proxy s))
    in object [prefix .= toJSON x]

data Meta = Meta
  { _metaApiVersion :: Int
  , _metaExecutionTime :: Text
  , _metaServerTimezone :: Text
  , _metaServerTime :: Int
  } deriving (Show, Eq)

makeLenses ''Meta

instance FromJSON Meta where
  parseJSON (Object v) =
    Meta <$> v .: "api_version" <*> v .: "execution_time" <*>
    v .: "server_timezone" <*>
    v .: "server_time"
  parseJSON _ = mzero

instance ToJSON Meta where
  toJSON x =
    object
      [ "api_version" .= (x ^. metaApiVersion)
      , "execution_time" .= (x ^. metaExecutionTime)
      , "server_timezone" .= (x ^. metaServerTimezone)
      , "server_time" .= (x ^. metaServerTime)
      ]

data Status a
  = OK a
  | Error
  deriving (Eq, Show)

instance ToJSON a => ToJSON (Status a) where
  toJSON (OK x) = toJSON x
  toJSON Error = "error"

data Response a = Response
  { _responseStatus :: Status a
  , _responseMeta :: Meta
  , _responseStatusMessage :: Text
  } deriving (Show, Eq)

makeLenses ''Response

instance FromJSON a =>
         FromJSON (Response a) where
  parseJSON (Object v) = do
    (status :: Value) <- v .: "status"
    case status of
      (String "ok") ->
        Response <$> (OK <$> (parseJSON =<< v .: "data")) <*> v .: "@meta" <*>
        v .: "status_message"
      (String "error") ->
        Response <$> pure Error <*> v .: "@meta" <*> v .: "status_message"
      _ -> mzero
  parseJSON _ = mzero

instance ToJSON a =>
         ToJSON (Response a) where
  toJSON x =
    object
      ([ "status" .=
         (case x ^. responseStatus of
            (OK _) -> "ok" :: Text
            Error -> "error")
       , "@meta" .= (x ^. responseMeta)
       , "status_message" .= (x ^. responseStatusMessage)
       ] ++
       (case x ^. responseStatus of
          (OK y) -> ["data" .= toJSON y]
          Error -> []))
