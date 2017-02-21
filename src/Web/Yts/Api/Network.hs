{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Yts.Api.Network
  ( send
  , collection
  , single
  ) where

import Protolude hiding (try)

import Control.Exception.Safe
import Control.Lens hiding ((&), from)
import Data.Aeson hiding (Error)
import GHC.TypeLits
import Network.HTTP.Client hiding (Response, Request, responseStatus)
import Network.HTTP.Types
import Web.Yts.Api.Pager.Types
import Web.Yts.Api.Request
import Web.Yts.Api.Response

send
  :: forall a b m.
     ( MonadIO m
     , MonadReader Manager m
     , MonadCatch m
     , MonadThrow m
     , ApiRequest a
     , FromJSON b
     )
  => a -> m (Either Text b)
send req = do
  let url =
        "https://yts.ag" <> request req ^. requestPath <>
        renderQuery True (request req ^. requestQuery)
  httpRequest <- parseRequest . toS $ url
  mgr <- ask
  httpResponse <- try (liftIO (evaluate =<< httpLbs httpRequest mgr))
  case httpResponse of
    Right x ->
      let ret =
            bimap toS identity (eitherDecode' . responseBody $ x) :: Either Text b
      in case ret of
           Right y -> return . Right $ y
           Left err -> return . Left . toS $ err
    Left (err :: SomeException) -> return . Left $ (show err :: Text)

collection
  :: ( KnownSymbol s
     , s ~ PayloadKeyPrefix b
     , ApiRequest a
     , FromJSON b
     , MonadIO m
     , MonadReader Manager m
     , MonadCatch m
     , MonadThrow m
     )
  => a -> m (Either Text [b])
collection req = do
  ret <- send req
  case ret of
    Right response ->
      case response ^. responseStatus of
        OK (Paginated (Payload x) _ _ _) -> return $ Right x
        Error -> return $ Left $ response ^. responseStatusMessage
    Left err -> return $ Left err

single
  :: ( KnownSymbol s
     , s ~ PayloadKeyPrefix b
     , ApiRequest a
     , FromJSON b
     , MonadIO m
     , MonadReader Manager m
     , MonadCatch m
     , MonadThrow m
     )
  => a -> m (Either Text b)
single req = do
  ret <- send req
  case ret of
    Right response ->
      case response ^. responseStatus of
        OK (Payload x) -> return $ Right x
        Error -> return $ Left $ response ^. responseStatusMessage
    Left err -> return $ Left err
