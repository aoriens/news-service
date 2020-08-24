module Web.AppURLSpec
  ( spec
  ) where

import qualified Core.Image as I
import qualified Data.ByteString.Char8 as BC
import Data.String
import Network.URI
import Test.Hspec
import qualified Web.AppURL as X

spec :: Spec
spec = do
  describe "toRelativeURL" $ do
    it "should return something that fromRelativeURL should decode back" $ do
      let appURL = defaultAppURL
          r = X.fromRelativeURL $ X.toRelativeURL appURL
      r `shouldBe` Just appURL
  describe "render'" $ do
    it "should return https URL if cfUseHTTPS is True" $ do
      let config = defaultConfig {X.cfUseHTTPS = True}
          r = X.render' config defaultAppURL
      uriScheme <$> parseURI (BC.unpack r) `shouldBe` Just "https:"
    it "should return http URL if cfUseHTTPS is False" $ do
      let config = defaultConfig {X.cfUseHTTPS = False}
          r = X.render' config defaultAppURL
      uriScheme <$> parseURI (BC.unpack r) `shouldBe` Just "http:"
    it "should return URL with domain from cfDomain" $ do
      let expectedDomain = "news.example.org"
          config = defaultConfig {X.cfDomain = fromString expectedDomain}
          r = X.render' config defaultAppURL
          domain = uriRegName <$> (uriAuthority =<< parseURI (BC.unpack r))
      domain `shouldBe` Just expectedDomain
    it "should return URL with port from cfPort if is not Nothing" $ do
      let expectedPort = 42
          config = defaultConfig {X.cfPort = Just expectedPort}
          r = X.render' config defaultAppURL
          port = uriPort <$> (uriAuthority =<< parseURI (BC.unpack r))
      port `shouldBe` Just (':' : show expectedPort)
    it "should return URL without port when cfPort = Nothing" $ do
      let config = defaultConfig {X.cfPort = Nothing}
          r = X.render' config defaultAppURL
          port = uriPort <$> (uriAuthority =<< parseURI (BC.unpack r))
      port `shouldBe` Just ""
    it "should return URL without fragment, query, username, or password" $ do
      let r = X.render' defaultConfig defaultAppURL
          uri = parseURI (BC.unpack r)
      uriFragment <$> uri `shouldBe` Just ""
      uriQuery <$> uri `shouldBe` Just ""
      uriUserInfo <$> (uriAuthority =<< uri) `shouldBe` Just ""

defaultAppURL :: X.AppURL
defaultAppURL = X.URLImage $ I.ImageId 1

defaultConfig :: X.Config
defaultConfig =
  X.Config
    {X.cfUseHTTPS = False, X.cfDomain = "example.com", X.cfPort = Just 152}
