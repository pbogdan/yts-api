{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TypesSpec where

import Protolude hiding (Meta)

import Data.Aeson hiding (Error)
import Data.String (String)
import Data.Time
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Web.Yts.Api

instance Arbitrary Quality where
  arbitrary = oneof [pure Q720P, pure Q1080P, pure Q3D]

instance Arbitrary Torrent where
  arbitrary =
    Torrent <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
    arbitrary <*>
    (parseTimeOrError True defaultTimeLocale "%Y-%m-%d %I:%M:%S" .
     formatTime defaultTimeLocale "%Y-%m-%d %I:%M:%S" <$>
     (arbitrary :: Gen UTCTime)) <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary

instance Arbitrary Movie where
  arbitrary =
    Movie <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    (parseTimeOrError True defaultTimeLocale "%Y-%m-%d %I:%M:%S" .
     formatTime defaultTimeLocale "%Y-%m-%d %I:%M:%S" <$>
     (arbitrary :: Gen UTCTime)) <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary

instance Arbitrary Meta where
  arbitrary = Meta <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a =>
         Arbitrary (Status a) where
  arbitrary = oneof [OK <$> arbitrary, pure Error]

instance Arbitrary a =>
         Arbitrary (Response a) where
  arbitrary = do
    status <- arbitrary :: Gen (Status a)
    Response <$> pure status <*> arbitrary <*> arbitrary

instance (Arbitrary a, s ~ PayloadKeyPrefix a, KnownSymbol s) =>
         Arbitrary (Payload s a) where
  arbitrary = Payload <$> arbitrary

instance (Arbitrary a, s ~ PayloadKeyPrefix a, KnownSymbol s) =>
         Arbitrary (Paginated s a) where
  arbitrary = Paginated <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = hspec spec

{-# ANN spec ("HLint: ignore Redundant do" :: Text) #-}
spec :: Spec
spec =
  describe "Response" $ do
    it "Payload: encode . decode == id" $ do
      property $ \(response :: Response (Payload "movie" Movie)) ->
        eitherDecode (encode response) `shouldBe`
        (Right response :: Either String (Response (Payload "movie" Movie)))
    it "Paginated: encode . decode == id" $ do
      property $ \(response :: Response (Paginated "movie" [Movie])) ->
        eitherDecode (encode response) `shouldBe`
        (Right response :: Either String (Response (Paginated "movie" [Movie])))
