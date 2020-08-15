module Web.QueryParameterSpec
  ( spec
  ) where

import Data.Int
import Test.Hspec
import Web.QueryParameter

spec :: Spec
spec =
  describe "parseQueryParameter" $ do
    it "should parse values in range" $ do
      parseQueryParameter (Just "1") `shouldBe` Just (1 :: Int)
      parseQueryParameter (Just "1") `shouldBe` Just (1 :: Int32)
    it "should not parse from Nothing" $ do
      parseQueryParameter Nothing `shouldBe` (Nothing :: Maybe Int)
      parseQueryParameter Nothing `shouldBe` (Nothing :: Maybe Int32)
    it "should not parse from malformed values" $ do
      parseQueryParameter (Just "") `shouldBe` (Nothing :: Maybe Int)
      parseQueryParameter (Just "q") `shouldBe` (Nothing :: Maybe Int)
      parseQueryParameter (Just "1q") `shouldBe` (Nothing :: Maybe Int)
      parseQueryParameter (Just "1.0") `shouldBe` (Nothing :: Maybe Int)
      parseQueryParameter (Just "") `shouldBe` (Nothing :: Maybe Int32)
      parseQueryParameter (Just "q") `shouldBe` (Nothing :: Maybe Int32)
      parseQueryParameter (Just "1.0") `shouldBe` (Nothing :: Maybe Int32)
      parseQueryParameter (Just "1q") `shouldBe` (Nothing :: Maybe Int32)
