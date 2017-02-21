{-# OPTIONS_GHC -fno-warn-unused-binds#-}

module Web.Yts.Api.Example
  (
  ) where

import           Protolude

import           Control.Lens
import           Control.Monad.Trans.Yts
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Pipes
import qualified Pipes.Prelude as P
import           Web.Yts.Api

example1 :: IO ()
example1 = do
  mgr <- newManager tlsManagerSettings
  runYtsApiT mgr $ do
    let req = moviesReq
    runEffect (paginate req >-> P.mapM_ (liftIO . print . view movieId))

example2 :: IO ()
example2 = do
  mgr <- newManager tlsManagerSettings
  runYtsApiT mgr $ do
    let req = SuggestionsReq 1
    resOrErr <- collection req
    case resOrErr of
      Right res -> for_ res (liftIO . print . view movieId)
      Left err -> putText $ "an error occurred: " <> err
