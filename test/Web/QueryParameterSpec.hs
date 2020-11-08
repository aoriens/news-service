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
import Web.QueryParameter

spec :: Spec
spec = do
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
  describe "lookupRawQueryParameter" $ do
    it "should find an existing key with a value" $ do
      let parser = lookupRawQueryParameter "target"
          query = [("target", Just "value")]
          r = parseQuery query parser
      r `shouldBe` Right (Just (Just "value"))
    it "should find an existing key with a missing value" $ do
      let parser = lookupRawQueryParameter "target"
          query = [("target", Nothing)]
          r = parseQuery query parser
      r `shouldBe` Right (Just Nothing)
    it "should return Nothing for missing key" $ do
      let parser = lookupRawQueryParameter "target"
          query = [("nonmatching", Just "value")]
          r = parseQuery query parser
      r `shouldBe` Right Nothing
  describe "lookupQueryParameter" $ do
    it "should parse a valid value for a found key" $ do
      let parser = lookupQueryParameter "target"
          query = [("target", Just "1")]
          r = parseQuery query parser
      r `shouldBe` Right (Just (1 :: Int))
    it "should return Nothing for a missing key" $ do
      let parser = lookupQueryParameter "target" :: QueryParser (Maybe Int)
          query = [("nonmatching", Just "1")]
          r = parseQuery query parser
      r `shouldBe` Right Nothing
    it "should return BadValue when parsing fails" $ do
      let parser =
            lookupQueryParameter "target" :: QueryParser (Maybe DoesNotParse)
          query = [("target", Just "")]
          r = parseQuery query parser
      r `shouldBe` Left (BadValue "target" (Just ""))
  describe "requireQueryParameter" $ do
    it "should parse a valid value for a found key" $ do
      let parser = requireQueryParameter "target"
          query = [("target", Just "1")]
          r = parseQuery query parser
      r `shouldBe` Right (1 :: Int)
    it "should return MissingKey for a missing key" $ do
      let parser = requireQueryParameter "target" :: QueryParser Int
          query = [("nonmatching", Just "1")]
          r = parseQuery query parser
      r `shouldBe` Left (MissingKey "target")
    it "should return BadValue when parsing fails" $ do
      let parser = requireQueryParameter "target" :: QueryParser DoesNotParse
          query = [("target", Just "")]
          r = parseQuery query parser
      r `shouldBe` Left (BadValue "target" (Just ""))
  describe "parseQuery" $ do
    it "should not evaluate query items after all keys are found" $ do
      let query = [("a", Nothing), ("b", Nothing), error "Must not evaluate"]
          parser =
            liftA2
              (,)
              (lookupRawQueryParameter "a")
              (lookupRawQueryParameter "b")
          r = parseQuery query parser
      -- assert: does not throw
      void $ evaluate $ force r
    it "should find all parameters if they are in the query" $
      property $ \keys -> do
        let bsKeys = map (BS8.pack . getASCIIString) keys
            query = map (, Nothing) bsKeys
            parser = traverse lookupRawQueryParameter bsKeys
            (Right rs) = parseQuery query parser
        rs `shouldSatisfy` all (== Just Nothing)
    it "should not evaluate the query when using pure parser" $
      property $ \x -> do
        let _ = x :: Int
            query = error "Must not evaluate"
            parser = pure x
            r = parseQuery query parser
      -- assert: does not throw
        void $ evaluate $ force r
    it "should not evaluate the query when using liftA2 of pure parsers" $
      property $ \(x, y) -> do
        let _ = (x, y) :: (Int, Int)
            query = error "Must not evaluate"
            parser1 = pure x
            parser = liftA2 (+) parser1 (pure y)
            r = parseQuery query parser
      -- assert: does not throw
        void $ evaluate $ force r
    it "should not evaluate the query when using fmap over a pure parser" $
      property $ \x -> do
        let _ = x :: Int
            query = error "Must not evaluate"
            parser1 = pure x
            parser = (10 +) <$> parser1
            r = parseQuery query parser
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

instance QueryParameter DoesNotParse where
  parseQueryParameter _ = Nothing
