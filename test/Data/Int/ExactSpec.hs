module Data.Int.ExactSpec
  ( spec
  ) where

import Data.Int
import Data.Int.Exact
import Data.Word
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "fromIntegralExact" $ do
    it "should succeed when converting to the same type" $
      property $ \x ->
        (fromIntegralExact (x :: Int) :: Maybe Int) `shouldBe` Just x
    it "should succeed when converting to a wider type" $
      property $ \x ->
        (fromIntegralExact (x :: Int8) :: Maybe Int16) `shouldBe`
        Just (fromIntegral x)
    it "should convert to a narrower type if preserves exact value" $ do
      (fromIntegralExact (127 :: Int) :: Maybe Int8) `shouldBe` Just 127
      (fromIntegralExact (-128 :: Int) :: Maybe Int8) `shouldBe` Just (-128)
      (fromIntegralExact (255 :: Int) :: Maybe Word8) `shouldBe` Just 255
      (fromIntegralExact (0 :: Int) :: Maybe Word) `shouldBe` Just 0
    it "should return Nothing when exact conversion is impossible" $ do
      (fromIntegralExact (128 :: Int) :: Maybe Int8) `shouldBe` Nothing
      (fromIntegralExact (-129 :: Int) :: Maybe Int8) `shouldBe` Nothing
      (fromIntegralExact (256 :: Int) :: Maybe Word8) `shouldBe` Nothing
      (fromIntegralExact (-1 :: Int) :: Maybe Word) `shouldBe` Nothing
