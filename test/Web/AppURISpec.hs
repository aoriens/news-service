module Web.AppURISpec
  ( spec
  ) where

import qualified Core.Image as I
import Data.String
import qualified Data.Text as T
import Network.URI
import Test.Hspec
import qualified Web.AppURI as X

spec :: Spec
spec = do
  describe "toRelativeURI" $ do
    it "should return something that fromRelativeURI should decode back" $ do
      let appURI = defaultAppURI
          r = X.fromRelativeURI $ X.toRelativeURI appURI
      r `shouldBe` Just appURI
  describe "render'" $ do
    it "should return https URI if cfUseHTTPS is True" $ do
      let config = defaultConfig {X.cfUseHTTPS = True}
          r = X.render config defaultAppURI
      uriScheme <$> parseURI (T.unpack r) `shouldBe` Just "https:"
    it "should return http URI if cfUseHTTPS is False" $ do
      let config = defaultConfig {X.cfUseHTTPS = False}
          r = X.render config defaultAppURI
      uriScheme <$> parseURI (T.unpack r) `shouldBe` Just "http:"
    it "should return URI with domain from cfDomain" $ do
      let expectedDomain = "news.example.org"
          config = defaultConfig {X.cfDomain = fromString expectedDomain}
          r = X.render config defaultAppURI
          domain = uriRegName <$> (uriAuthority =<< parseURI (T.unpack r))
      domain `shouldBe` Just expectedDomain
    it "should return URI without fragment, query, username, or password" $ do
      let r = X.render defaultConfig defaultAppURI
          uri = parseURI (T.unpack r)
      uriFragment <$> uri `shouldBe` Just ""
      uriQuery <$> uri `shouldBe` Just ""
      uriUserInfo <$> (uriAuthority =<< uri) `shouldBe` Just ""

defaultAppURI :: X.AppURI
defaultAppURI = X.URIImage $ I.ImageId 1

defaultConfig :: X.Config
defaultConfig = X.Config {X.cfUseHTTPS = False, X.cfDomain = "example.com"}
