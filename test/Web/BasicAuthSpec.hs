module Web.BasicAuthSpec
  ( spec
  ) where

import qualified Data.ByteString as B
import Data.ByteString.Base64
import Data.Either
import Network.Wai as Wai
import Test.Hspec
import Web.BasicAuth

spec :: Spec
spec =
  describe "credentialsFromRequest" $ do
    it "should decode canonically encoded correct header" $ do
      let request =
            requestWithAuthenticationHeader $
            "Basic " <> encodeBase64' "user:password"
          r = credentialsFromRequest request
      r `shouldBe` Right (Just ("user", "password"))
    it "should decode correct header with an unusual case in Basic prefix" $ do
      let request =
            requestWithAuthenticationHeader $
            "bASic " <> encodeBase64' "user:password"
          r = credentialsFromRequest request
      r `shouldBe` Right (Just ("user", "password"))
    it "should decode correct header with extra spaces" $ do
      let request =
            requestWithAuthenticationHeader $
            " \v \n  Basic    \t \v  " <>
            encodeBase64' "user:password" <> " \n "
          r = credentialsFromRequest request
      r `shouldBe` Right (Just ("user", "password"))
    it "should decode correct header with an empty user" $ do
      let request =
            requestWithAuthenticationHeader $
            "Basic " <> encodeBase64' ":password"
          r = credentialsFromRequest request
      r `shouldBe` Right (Just ("", "password"))
    it "should decode correct header with an empty password" $ do
      let request =
            requestWithAuthenticationHeader $ "Basic " <> encodeBase64' "user:"
          r = credentialsFromRequest request
      r `shouldBe` Right (Just ("user", ""))
    it "should decode correct header with an empty user and a password" $ do
      let request =
            requestWithAuthenticationHeader $ "Basic " <> encodeBase64' ":"
          r = credentialsFromRequest request
      r `shouldBe` Right (Just ("", ""))
    it "should decode correct header with a password containing a colon" $ do
      let request =
            requestWithAuthenticationHeader $ "Basic " <> encodeBase64' "a:b:c"
          r = credentialsFromRequest request
      r `shouldBe` Right (Just ("a", "b:c"))
    it "should return Right Nothing if no Authorization header found" $ do
      let r = credentialsFromRequest defaultRequest
      r `shouldBe` Right Nothing
    it "should return Left () for header consising of spaces only" $ do
      let request = requestWithAuthenticationHeader "  \t  \n\f\r  \t "
          r = credentialsFromRequest request
      r `shouldSatisfy` isLeft
    it "should return Left () for a single-word header" $ do
      let request = requestWithAuthenticationHeader "Basic "
          r = credentialsFromRequest request
      r `shouldSatisfy` isLeft
    it "should return Left () for a triple-word header" $ do
      let request =
            requestWithAuthenticationHeader $
            "Basic " <> correctToken <> " " <> correctToken
          correctToken = encodeBase64' "a:b"
          r = credentialsFromRequest request
      r `shouldSatisfy` isLeft
    it "should return Left () for a non-basic auth" $ do
      let request =
            requestWithAuthenticationHeader $ "Digest " <> encodeBase64' "a:b"
          r = credentialsFromRequest request
      r `shouldSatisfy` isLeft
    it "should return Left () for an incorrect base64" $ do
      let request = requestWithAuthenticationHeader "Basic Og=!"
          r = credentialsFromRequest request
      r `shouldSatisfy` isLeft
    it "should return Left () when credentials does not contain a colon" $ do
      let request =
            requestWithAuthenticationHeader $ "Basic " <> encodeBase64' "ab"
          r = credentialsFromRequest request
      r `shouldSatisfy` isLeft

requestWithAuthenticationHeader :: B.ByteString -> Wai.Request
requestWithAuthenticationHeader value =
  Wai.defaultRequest {requestHeaders = [("Authorization", value)]}
