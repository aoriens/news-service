{-# LANGUAGE TupleSections #-}

module Web.QueryParameterSpec
  ( spec
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.Int
import Data.Proxy
import Prelude hiding (lookup)
import Test.Hspec
import Test.QuickCheck
import qualified Web.QueryParameter as QueryParameter

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse values in range" $ do
      QueryParameter.parse (Just "1") `shouldBe` Just (1 :: Int)
      QueryParameter.parse (Just "1") `shouldBe` Just (1 :: Int32)
    it "should not parse from Nothing" $ do
      QueryParameter.parse Nothing `shouldBe` (Nothing :: Maybe Int)
      QueryParameter.parse Nothing `shouldBe` (Nothing :: Maybe Int32)
    it "should not parse from malformed values" $ do
      QueryParameter.parse (Just "") `shouldBe` (Nothing :: Maybe Int)
      QueryParameter.parse (Just "q") `shouldBe` (Nothing :: Maybe Int)
      QueryParameter.parse (Just "1q") `shouldBe` (Nothing :: Maybe Int)
      QueryParameter.parse (Just "1.0") `shouldBe` (Nothing :: Maybe Int)
      QueryParameter.parse (Just "") `shouldBe` (Nothing :: Maybe Int32)
      QueryParameter.parse (Just "q") `shouldBe` (Nothing :: Maybe Int32)
      QueryParameter.parse (Just "1.0") `shouldBe` (Nothing :: Maybe Int32)
      QueryParameter.parse (Just "1q") `shouldBe` (Nothing :: Maybe Int32)
    it "should not parse from values out of range" $ do
      QueryParameter.parse (Just (greaterMaxBoundString (Proxy :: Proxy Int32))) `shouldBe`
        (Nothing :: Maybe Int32)
      QueryParameter.parse (Just (lessMinBoundString (Proxy :: Proxy Int32))) `shouldBe`
        (Nothing :: Maybe Int32)
      QueryParameter.parse (Just (greaterMaxBoundString (Proxy :: Proxy Int))) `shouldBe`
        (Nothing :: Maybe Int)
      QueryParameter.parse (Just (lessMinBoundString (Proxy :: Proxy Int))) `shouldBe`
        (Nothing :: Maybe Int)
  describe "lookupRaw" $ do
    it "should find an existing key with a value" $ do
      let parser = QueryParameter.lookupRaw "k"
          query = [("k", Just "value")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right (Just (Just "value"))
    it
      "should find an existing key with a value if collectRaw is invoked with the same key too" $ do
      let parser =
            QueryParameter.collectRaw "k" *> QueryParameter.lookupRaw "k" <*
            QueryParameter.collectRaw "k"
          query = [("k", Just "value")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right (Just (Just "value"))
    it "should find an existing key with a missing value" $ do
      let parser = QueryParameter.lookupRaw "k"
          query = [("k", Nothing)]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right (Just Nothing)
    it "should return Nothing for missing key" $ do
      let parser = QueryParameter.lookupRaw "k"
          query = [("bad", Just "value")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right Nothing
  describe "exists" $ do
    it "should return True for an existing key with a value" $ do
      let parser = QueryParameter.exists "k"
          query = [("k", Just "value")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right True
    it "should return True for an existing key with a missing value" $ do
      let parser = QueryParameter.exists "k"
          query = [("k", Nothing)]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right True
    it "should return False for a missing key" $ do
      let parser = QueryParameter.exists "k"
          query = [("bad", Just "value")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right False
  describe "QueryParameter.collectRaw" $ do
    it "should find all existing values for key value while keeping order" $ do
      let parser = QueryParameter.collectRaw "k"
          query = [("k", Just "1"), ("k", Just "1"), ("k", Just "2")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right [Just "1", Just "1", Just "2"]
    it
      "should find all existing values for the key while keeping order if QueryParameter.collectRaw invoked twice with the same key" $ do
      let parser =
            QueryParameter.collectRaw "k" *> QueryParameter.collectRaw "k"
          query = [("k", Just "1"), ("k", Just "1"), ("k", Just "2")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right [Just "1", Just "1", Just "2"]
    it
      "should find all existing values for the key while keeping order if QueryParameter.lookupRaw invoked with the same key too" $ do
      let parser =
            QueryParameter.lookupRaw "k" *> QueryParameter.collectRaw "k" <*
            QueryParameter.lookupRaw "k"
          query = [("k", Just "1"), ("k", Just "1"), ("k", Just "2")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right [Just "1", Just "1", Just "2"]
    it "should find all existing keys with missing values" $ do
      let parser = QueryParameter.collectRaw "k"
          query = [("k", Nothing), ("k", Just "")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right [Nothing, Just ""]
    it "should return [] for missing key" $ do
      let parser = QueryParameter.collectRaw "k"
          query = [("bad", Just "value")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right []
    it "should not return values for non-matching keys" $ do
      let parser = QueryParameter.collectRaw "k"
          query = [("bad", Just "bad"), ("k", Just "good")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right [Just "good"]
  describe "lookup" $ do
    it "should parse a valid value for a found key" $ do
      let parser = QueryParameter.lookup "k"
          query = [("k", Just "1")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right (Just (1 :: Int))
    it "should return Nothing for a missing key" $ do
      let parser =
            QueryParameter.lookup "k" :: QueryParameter.Parser (Maybe Int)
          query = [("bad", Just "1")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right Nothing
    it "should return BadValue when parsing fails" $ do
      let parser =
            QueryParameter.lookup "k" :: QueryParameter.Parser (Maybe DoesNotParse)
          query = [("k", Just "")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Left (QueryParameter.BadValue "k" (Just ""))
  describe "require" $ do
    it "should parse a valid value for a found key" $ do
      let parser = QueryParameter.require "k"
          query = [("k", Just "1")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right (1 :: Int)
    it "should return MissingKey for a missing key" $ do
      let parser = QueryParameter.require "k" :: QueryParameter.Parser Int
          query = [("bad", Just "1")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Left (QueryParameter.MissingKey "k")
    it "should return BadValue when parsing fails" $ do
      let parser =
            QueryParameter.require "k" :: QueryParameter.Parser DoesNotParse
          query = [("k", Just "")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Left (QueryParameter.BadValue "k" (Just ""))
  describe "collect" $ do
    it "should parse all values if they are valid for a found key" $ do
      let parser = QueryParameter.collect "k"
          query = [("k", Just "1"), ("k", Just "2")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right ([1, 2] :: [Int])
    it "should return [] for a missing key" $ do
      let parser = QueryParameter.collect "k" :: QueryParameter.Parser [Int]
          query = [("bad", Just "1")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Right []
    it
      "should return BadValue if the last parameter with the key among several ones cannot be parsed" $ do
      let parser = QueryParameter.collect "k" :: QueryParameter.Parser [Int]
          query = [("k", Just "1"), ("k", Just "bad")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Left (QueryParameter.BadValue "k" (Just "bad"))
    it
      "should return BadValue if the first parameter with the key among several ones cannot be parsed" $ do
      let parser = QueryParameter.collect "k" :: QueryParameter.Parser [Int]
          query = [("k", Just "bad"), ("k", Just "1")]
          r = QueryParameter.parseQuery query parser
      r `shouldBe` Left (QueryParameter.BadValue "k" (Just "bad"))
  describe "QueryParameter.parseQuery" $ do
    it "should not evaluate query items after all keys are found" $ do
      let query = [("a", Nothing), ("b", Nothing), error "Must not evaluate"]
          parser =
            liftA2
              (,)
              (QueryParameter.lookupRaw "a")
              (QueryParameter.lookupRaw "b")
          r = QueryParameter.parseQuery query parser
      -- assert: does not throw
      void $ evaluate $ force r
    it "should find all parameters if they are in the query" $
      property $ \keys -> do
        let bsKeys = map (BS8.pack . getASCIIString) keys
            query = map (, Nothing) bsKeys
            parser = traverse QueryParameter.lookupRaw bsKeys
            (Right rs) = QueryParameter.parseQuery query parser
        rs `shouldSatisfy` all (== Just Nothing)
    it "should not evaluate the query when using pure parser" $
      property $ \x -> do
        let _ = x :: Int
            query = error "Must not evaluate"
            parser = pure x
            r = QueryParameter.parseQuery query parser
      -- assert: does not throw
        void $ evaluate $ force r
    it "should not evaluate the query when using liftA2 of pure parsers" $
      property $ \(x, y) -> do
        let _ = (x, y) :: (Int, Int)
            query = error "Must not evaluate"
            parser1 = pure x
            parser = liftA2 (+) parser1 (pure y)
            r = QueryParameter.parseQuery query parser
      -- assert: does not throw
        void $ evaluate $ force r
    it "should not evaluate the query when using fmap over a pure parser" $
      property $ \x -> do
        let _ = x :: Int
            query = error "Must not evaluate"
            parser1 = pure x
            parser = (10 +) <$> parser1
            r = QueryParameter.parseQuery query parser
      -- assert: does not throw
        void $ evaluate $ force r

greaterMaxBoundString ::
     (Bounded a, Integral a, Show a) => Proxy a -> BS8.ByteString
greaterMaxBoundString p =
  BS8.pack . show $ toInteger (maxBound `asProxyTypeOf` p) + 1

lessMinBoundString ::
     (Bounded a, Integral a, Show a) => Proxy a -> BS8.ByteString
lessMinBoundString p =
  BS8.pack . show $ toInteger (minBound `asProxyTypeOf` p) - 1

data DoesNotParse =
  DoesNotParse
  deriving (Show, Eq)

instance QueryParameter.Parses DoesNotParse where
  parse _ = Nothing
