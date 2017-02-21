{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Control.Monad.Trans.Yts where

import Protolude

import Control.Monad.Catch
import Control.Monad.Trans.Class

newtype YtsApiT r (m :: * -> *) a = YtsApiT
  { unYtsApiT :: ReaderT r m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader r
             , MonadCatch
             , MonadThrow
             , MonadIO
             , MonadTrans
             )

runYtsApiT
  :: Monad m
  => r -> YtsApiT r m a -> m a
runYtsApiT env (YtsApiT x) = runReaderT x env
