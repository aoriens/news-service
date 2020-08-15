module Web.QueryParameterSpec
  ( spec
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Int
import Data.Proxy
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
    it "should not parse from values out of range" $ do
      parseQueryParameter (Just (greaterMaxBoundString (Proxy :: Proxy Int32))) `shouldBe`
        (Nothing :: Maybe Int32)
      parseQueryParameter (Just (lessMinBoundString (Proxy :: Proxy Int32))) `shouldBe`
        (Nothing :: Maybe Int32)
      parseQueryParameter (Just (greaterMaxBoundString (Proxy :: Proxy Int))) `shouldBe`
        (Nothing :: Maybe Int)
      parseQueryParameter (Just (lessMinBoundString (Proxy :: Proxy Int))) `shouldBe`
        (Nothing :: Maybe Int)

greaterMaxBoundString ::
     (Bounded a, Integral a, Show a) => Proxy a -> BS8.ByteString
greaterMaxBoundString p =
  BS8.pack . show $ toInteger (maxBound `asProxyTypeOf` p) + 1

lessMinBoundString ::
     (Bounded a, Integral a, Show a) => Proxy a -> BS8.ByteString
lessMinBoundString p =
  BS8.pack . show $ toInteger (minBound `asProxyTypeOf` p) - 1
