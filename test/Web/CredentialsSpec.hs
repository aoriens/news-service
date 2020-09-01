module Web.CredentialsSpec
  ( spec
  ) where

import qualified Core.Authentication as Core
import Core.User
import Data.ByteString.Base64
import Data.String
import Test.Hspec
import Test.QuickCheck
import Web.Credentials

spec :: Spec
spec = do
  describe "presentCredentials" $ do
    it "should emit something that readCredentials should read back" $
      property $ \(uid, token) -> do
        let sourceCreds =
              Core.TokenCredentials
                (UserId uid)
                (Core.SecretToken $ fromString token)
            creds = presentCredentials sourceCreds
            r = readCredentials creds
        r `shouldBe` Just sourceCreds
    it
      "should present credentials as comma-separated pair of UserId and a base64-encoded token" $ do
      let coreCreds = Core.TokenCredentials (UserId 1) (Core.SecretToken "qwe")
          creds = presentCredentials coreCreds
      creds `shouldBe` WebToken ("1," <> encodeBase64' "qwe")
  describe "readCredentials" $ do
    it "should decode correct credentials" $ do
      let creds = WebToken $ "1," <> encodeBase64' "qwe"
          coreCreds = readCredentials creds
      coreCreds `shouldBe`
        Just (Core.TokenCredentials (UserId 1) (Core.SecretToken "qwe"))
    it "should return Nothing if no comma found" $ do
      let creds = WebToken "1"
          coreCreds = readCredentials creds
      coreCreds `shouldBe` Nothing
    it "should return Nothing for non-numeric login" $ do
      let creds = WebToken $ "q," <> encodeBase64' "qwe"
          coreCreds = readCredentials creds
      coreCreds `shouldBe` Nothing
    it "should return Nothing for too large numeric value in login" $ do
      let creds = WebToken $ "999999999999," <> encodeBase64' "qwe"
          coreCreds = readCredentials creds
      coreCreds `shouldBe` Nothing
    it "should return Nothing for invalid base64 in the password" $ do
      let creds = WebToken "1,q!"
          coreCreds = readCredentials creds
      coreCreds `shouldBe` Nothing
