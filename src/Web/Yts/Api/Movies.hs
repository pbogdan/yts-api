{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Yts.Api.Movies
  ( Quality(..)
  , Torrent(..)
  , torrentDateUploadedUnix
  , torrentSeeds
  , torrentHash
  , torrentSize
  , torrentUrl
  , torrentDateUploaded
  , torrentQuality
  , torrentSizeBytes
  , torrentPeers
  , Movie(..)
  , movieId
  , moviesReqGenre
  , moviesReqLimit
  , moviesReqMinimumRating
  , moviesReqOrderBy
  , moviesReqQuality
  , moviesReqQueryTerm
  , moviesReqWithRtRatings
  , movieReqMovieId
  , movies
  , movie
  , moviesReq
  , SuggestionsReq(..)
  , suggestionsReqMovieId
  ) where

import Protolude

import Control.Arrow
import Control.Exception.Safe
import Control.Lens hiding ((&), from, (.=))
import Data.Aeson
import Data.Time
import GHC.TypeLits
import Network.HTTP.Client hiding (Response, Request, responseStatus)
import Network.HTTP.Types
import Web.Yts.Api.Network
import Web.Yts.Api.Pager
import Web.Yts.Api.Request
import Web.Yts.Api.Response

data Quality
  = Q720P
  | Q1080P
  | Q3D
  deriving (Eq, Show)

instance FromJSON Quality where
  parseJSON (String "720p") = pure Q720P
  parseJSON (String "1080p") = pure Q1080P
  parseJSON (String "3D") = pure Q3D
  parseJSON _ = mzero

instance ToJSON Quality where
  toJSON Q720P = "720p"
  toJSON Q1080P = "1080p"
  toJSON Q3D = "3D"

data Torrent = Torrent
  { _torrentDateUploadedUnix :: Int
  , _torrentSeeds :: Int
  , _torrentHash :: Text
  , _torrentSize :: Text
  , _torrentUrl :: Text
  , _torrentDateUploaded :: UTCTime
  , _torrentQuality :: Quality
  , _torrentSizeBytes :: Int
  , _torrentPeers :: Int
  } deriving (Show, Eq)

makeLenses ''Torrent

instance FromJSON Torrent where
  parseJSON (Object v) =
    Torrent <$> v .: "date_uploaded_unix" <*> v .: "seeds" <*> v .: "hash" <*>
    v .: "size" <*>
    v .: "url" <*>
    (parseTimeM True defaultTimeLocale "%Y-%m-%d %I:%M:%S" =<<
     v .: "date_uploaded") <*>
    v .: "quality" <*>
    v .: "size_bytes" <*>
    v .: "peers"
  parseJSON _ = mzero

instance ToJSON Torrent where
  toJSON x =
    object
      [ "date_uploaded_unix" .= (x ^. torrentDateUploadedUnix)
      , "seeds" .= (x ^. torrentSeeds)
      , "hash" .= (x ^. torrentHash)
      , "size" .= (x ^. torrentSize)
      , "url" .= (x ^. torrentUrl)
      , "date_uploaded" .=
        formatTime
          defaultTimeLocale
          "%Y-%m-%d %I:%M:%S"
          (x ^. torrentDateUploaded)
      , "quality" .= (x ^. torrentQuality)
      , "size_bytes" .= (x ^. torrentSizeBytes)
      , "peers" .= (x ^. torrentPeers)
      ]

data Movie = Movie
  { _movieSummary :: Text
  , _movieRuntime :: Int
  , _movieMediumCoverImage :: Text
  , _movieRating :: Double
  , _movieLargeCoverImage :: Maybe Text
  , _movieState :: Maybe Text
  , _movieDescriptionFull :: Text
  , _movieSlug :: Text
  , _movieTitleLong :: Text
  , _movieSmallCoverImage :: Text
  , _movieBackgroundImageOriginal :: Text
  , _movieImdbCode :: Text
  , _movieUrl :: Text
  , _movieDateUploaded :: UTCTime
  , _movieMpaRating :: Text
  , _movieTorrents :: [Torrent]
  , _movieYear :: Int
  , _movieSynopsis :: Text
  , _movieLanguage :: Text
  , _movieTitleEnglish :: Text
  , _movieId :: Int
  , _movieYtTrailerCode :: Text
  , _movieGenres :: [Text]
  , _movieTitle :: Text
  , _movieBackgroundImage :: Text
  } deriving (Show, Eq)

makeLenses ''Movie

instance FromJSON Movie where
  parseJSON (Object v) =
    Movie <$> (v .: "summary" <|> v .: "description_full") <*> v .: "runtime" <*>
    v .: "medium_cover_image" <*>
    v .: "rating" <*>
    v .:? "large_cover_image" <*>
    v .:? "state" <*>
    v .: "description_full" <*>
    v .: "slug" <*>
    v .: "title_long" <*>
    v .: "small_cover_image" <*>
    v .: "background_image_original" <*>
    v .: "imdb_code" <*>
    v .: "url" <*>
    (parseTimeM True defaultTimeLocale "%Y-%m-%d %I:%M:%S" =<<
     v .: "date_uploaded") <*>
    v .: "mpa_rating" <*>
    v .: "torrents" <*>
    v .: "year" <*>
    (v .: "synopsis" <|> v .: "description_intro") <*>
    v .: "language" <*>
    v .: "title_english" <*>
    v .: "id" <*>
    v .: "yt_trailer_code" <*>
    v .: "genres" <*>
    v .: "title" <*>
    v .: "background_image"
  parseJSON _ = mzero

instance ToJSON Movie where
  toJSON x =
    object
      [ "summary" .= (x ^. movieSummary)
      , "runtime" .= (x ^. movieRuntime)
      , "medium_cover_image" .= (x ^. movieMediumCoverImage)
      , "rating" .= (x ^. movieRating)
      , "large_cover_image" .= (x ^. movieLargeCoverImage)
      , "state" .= (x ^. movieState)
      , "description_full" .= (x ^. movieDescriptionFull)
      , "slug" .= (x ^. movieSlug)
      , "title_long" .= (x ^. movieTitleLong)
      , "small_cover_image" .= (x ^. movieSmallCoverImage)
      , "background_image_original" .= (x ^. movieBackgroundImageOriginal)
      , "imdb_code" .= (x ^. movieImdbCode)
      , "url" .= (x ^. movieUrl)
      , "date_uploaded" .=
        formatTime
          defaultTimeLocale
          "%Y-%m-%d %I:%M:%S"
          (x ^. movieDateUploaded)
      , "mpa_rating" .= (x ^. movieMpaRating)
      , "torrents" .= (x ^. movieTorrents)
      , "year" .= (x ^. movieYear)
      , "synopsis" .= (x ^. movieSynopsis)
      , "language" .= (x ^. movieLanguage)
      , "title_english" .= (x ^. movieTitleEnglish)
      , "id" .= (x ^. movieId)
      , "yt_trailer_code" .= (x ^. movieYtTrailerCode)
      , "genres" .= (x ^. movieGenres)
      , "title" .= (x ^. movieTitle)
      , "background_image" .= (x ^. movieBackgroundImage)
      ]

type instance PayloadKeyPrefix Movie = "movie"

movies
  :: ( KnownSymbol (PayloadKeyPrefix Movie)
     , MonadIO m
     , MonadReader Manager m
     , MonadCatch m
     , MonadThrow m
     )
  => MoviesReq -> m (Either Text [Movie])
movies = collection

movie
  :: ( KnownSymbol (PayloadKeyPrefix Movie)
     , MonadIO m
     , MonadReader Manager m
     , MonadCatch m
     , MonadThrow m
     )
  => MoviesReq -> m (Either Text Movie)
movie = single

data MoviesReq = MoviesReq
  { _moviesReqLimit :: Maybe Int
  , _moviesReqPage :: Maybe Int
  , _moviesReqQuality :: Maybe Quality
  , _moviesReqMinimumRating :: Maybe Double
  , _moviesReqQueryTerm :: Maybe Text
  , _moviesReqGenre :: Maybe Text
  , _moviesReqOrderBy :: Maybe Order
  , _moviesReqWithRtRatings :: Bool
  } deriving (Eq, Generic, Show)

makeLenses ''MoviesReq

moviesReq :: MoviesReq
moviesReq =
  MoviesReq Nothing Nothing Nothing Nothing Nothing Nothing Nothing True

instance ApiRequest MoviesReq where
  request x =
    mkRequest & requestQuery .~
    map (toS . snakeCase . drop 10 . toS *** identity) (toQueryString x) &
    requestPath .~
    "/api/v2/list_movies.json" &
    requestMethod .~
    methodGet

instance ApiPager MoviesReq where
  page' req (Paginated _ t c l) =
    let next = advance c t l
    in maybe Nothing (const $ Just (req & moviesReqPage .~ next)) next

data MovieReq = MovieReq
  { movieReqMovieId :: Int
  } deriving (Eq, Generic, Show)

makeLenses ''MovieReq

instance ApiRequest MovieReq where
  request x =
    mkRequest & requestQuery .~
    map (toS . snakeCase . drop 8 . toS *** identity) (toQueryString x) &
    requestPath .~
    "/api/v2/movie_details.json" & requestMethod .~
    methodGet

data SuggestionsReq = SuggestionsReq
  { _suggestionsReqMovieId :: Int
  } deriving (Eq, Generic, Show)

makeLenses ''SuggestionsReq

instance ApiRequest SuggestionsReq where
  request x =
    mkRequest & requestQuery .~
    map (toS . snakeCase . drop 15 . toS *** identity) (toQueryString x) &
    requestPath .~
    "/api/v2/movie_suggestions.json" &
    requestMethod .~
    methodGet
