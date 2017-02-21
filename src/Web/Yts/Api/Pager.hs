{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Yts.Api.Pager
  ( Paginated(..)
  , ApiPager(..)
  , advance
  , paginate
  ) where

import Protolude

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Data.Aeson hiding (Error)
import GHC.TypeLits
import Network.HTTP.Client (Manager)
import Pipes hiding (Proxy)
import Web.Yts.Api.Network
import Web.Yts.Api.Pager.Types
import Web.Yts.Api.Request
import Web.Yts.Api.Response

class ApiPager a where
  page' :: a -> Paginated s b -> Maybe a

advance :: Int -> Int -> Int -> Maybe Int
advance current total limit =
  if current * limit >= total
    then Nothing
    else Just (current + 1)

paginate
  :: ( ApiPager a
     , ApiRequest a
     , KnownSymbol (PayloadKeyPrefix b)
     , FromJSON b
     , MonadCatch m
     , MonadThrow m
     , MonadReader Manager m
     , MonadIO m
     )
  => a -> Producer b m ()
paginate x = do
  !y <- send x
  case y of
    Right response ->
      case response ^. responseStatus of
        OK p@(Paginated (Payload (ret :: [b])) _ _ _) -> do
          for_ ret yield
          maybe (pure ()) paginate (page' x p)
        Error -> return ()
    Left _ -> return ()
