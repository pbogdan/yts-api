{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Web.Yts.Api.Request
  ( Request(..)
  , ApiRequest(..)
  , mkRequest
  , requestMethod
  , requestPath
  , requestQuery
  , toQueryString
  , applyFirst
  , snakeCase
  , Order(..)
  ) where

import Protolude hiding (try)

import Control.Lens hiding ((&), from)
import Data.Char
import Data.String (String)
import GHC.Generics
import Network.HTTP.Types

data Request = Request
  { _requestQuery :: Query
  , _requestPath :: ByteString
  , _requestMethod :: Method
  } deriving (Eq, Show)

mkRequest :: Request
mkRequest = Request [] "" methodGet

makeLenses ''Request

class ApiRequest a where
  request :: a -> Request

toQueryString
  :: (ToQueryString' (Rep a), Generic a)
  => a -> Query
toQueryString = toQueryString' . from

class ToQueryString' f where
  toQueryString' :: f a -> Query

instance (Selector s, Show t) =>
         ToQueryString' (M1 S s (K1 i t)) where
  toQueryString' (M1 (K1 x)) =
    let key = selName (undefined :: (M1 S s (K1 i t) ()))
    in [(toS key, Just . toS $ (show x :: Text))]

instance {-# OVERLAPPING #-} (Selector s, Show t) =>
         ToQueryString' (M1 S s (K1 i (Maybe t))) where
  toQueryString' (M1 (K1 (Just x))) =
    let key = selName (undefined :: (M1 S s (K1 i t) ()))
    in [(toS key, Just . toS $ (show x :: Text))]
  toQueryString' (M1 (K1 Nothing)) = []

instance {-# OVERLAPPING #-}(Selector s) =>
         ToQueryString' (M1 S s (K1 i Bool)) where
  toQueryString' (M1 (K1 x)) =
    let key = selName (undefined :: (M1 S s (K1 i t) ()))
    in [ ( toS key
         , Just . toS $
           (show
              (if x
                 then 1 :: Int
                 else 0) :: Text))
       ]

instance (ToQueryString' a, ToQueryString' b) =>
         ToQueryString' (a :*: b) where
  toQueryString' (a :*: b) = toQueryString' a ++ toQueryString' b

instance ToQueryString' V1 where
  toQueryString' _ = []

instance ToQueryString' U1 where
  toQueryString' _ = []

instance (ToQueryString' a, ToQueryString' b) => ToQueryString' (a :+: b) where
  toQueryString' (L1 x) = toQueryString' x
  toQueryString' (R1 x) = toQueryString' x

instance (ToQueryString' f) =>
         ToQueryString' (M1 D x f) where
  toQueryString' (M1 x) = toQueryString' x

instance ToQueryString' f =>
         ToQueryString' (M1 C x f) where
  toQueryString' (M1 x) = toQueryString' x

data Order
  = ASC
  | DESC
  deriving (Eq, Show)

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ []     = []
applyFirst f [x]    = [f x]
applyFirst f (x:xs) = f x: xs

snakeCase :: String -> String
snakeCase = u . applyFirst toLower
    where u []                 = []
          u (x:xs) | isUpper x = '_' : toLower x : snakeCase xs
                   | otherwise = x : u xs
