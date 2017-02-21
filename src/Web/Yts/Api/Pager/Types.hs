{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Yts.Api.Pager.Types
  ( Paginated(..)
  ) where

import Protolude

import Control.Lens hiding ((.=))
import Data.Aeson hiding (Error)
import GHC.TypeLits
import Web.Yts.Api.Response


data Paginated s a = Paginated
  { _paginatedPayload :: Payload s a
  , _paginatedCount :: Int
  , _paginatedPageNumber :: Int
  , _paginatedLimit :: Int
  } deriving (Show, Eq)

makeLenses ''Paginated

instance (s ~ PayloadKeyPrefix a, KnownSymbol s, FromJSON a) =>
         FromJSON (Paginated s a) where
  parseJSON (Object v) =
    let prefix = toS (symbolVal (Proxy :: Proxy s))
    in Paginated <$> (Payload <$> (parseJSON =<< v .: (prefix <> "s"))) <*>
       (parseJSON =<< v .: (prefix <> ("_count" :: Text))) <*>
       (v .: "page_number" <|> pure 1) <*>
       (v .: "limit" <|> pure 20)
  parseJSON _ = mzero

instance (s ~ PayloadKeyPrefix a, KnownSymbol s, ToJSON a) =>
         ToJSON (Paginated s a) where
  toJSON x =
    let prefix = toS (symbolVal (Proxy :: Proxy s))
        (Payload p) = x ^. paginatedPayload
    in object
         [ (prefix <> "s") .= toJSON p
         , (prefix <> "_count") .= (x ^. paginatedCount)
         , "page_number" .= (x ^. paginatedPageNumber)
         , "limit" .= (x ^. paginatedLimit)
         ]

