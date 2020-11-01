{-# LANGUAGE LambdaCase #-}

module Web.AppURISpec
  ( spec
  ) where

import Core.Author
import Core.Category
import Core.Image
import Core.Tag
import Core.User
import Data.String
import qualified Data.Text as T
import Network.URI
import Test.Hspec
import Web.AppURI

spec :: Spec
spec = do
  describe "toRelativeURI" $ do
    it "should parse back all supported URIs after rendering them" $ do
      let uris = appURIsForAllPossibleConstructors
          results = map (fromRelativeURI . toRelativeURI) uris
      results `shouldBe` map Just uris
  describe "renderAppURI" $ do
    it "should return https URI if cfUseHTTPS is True" $ do
      let config = defaultConfig {cfUseHTTPS = True}
          r = renderAppURI config defaultAppURI
      uriScheme <$> parseURI (T.unpack r) `shouldBe` Just "https:"
    it "should return http URI if cfUseHTTPS is False" $ do
      let config = defaultConfig {cfUseHTTPS = False}
          r = renderAppURI config defaultAppURI
      uriScheme <$> parseURI (T.unpack r) `shouldBe` Just "http:"
    it "should return URI with domain from cfDomain" $ do
      let expectedDomain = "news.example.org"
          config = defaultConfig {cfDomain = fromString expectedDomain}
          r = renderAppURI config defaultAppURI
          domain = uriRegName <$> (uriAuthority =<< parseURI (T.unpack r))
      domain `shouldBe` Just expectedDomain
    it "should return URI without fragment, query, username, or password" $ do
      let r = renderAppURI defaultConfig defaultAppURI
          uri = parseURI (T.unpack r)
      uriFragment <$> uri `shouldBe` Just ""
      uriQuery <$> uri `shouldBe` Just ""
      uriUserInfo <$> (uriAuthority =<< uri) `shouldBe` Just ""
  describe "parseAppURI" $ do
    it
      "should parse back everything renderAppURI has rendered with the same config" $ do
      let config = defaultConfig {cfUseHTTPS = False, cfDomain = "a.com"}
          uris = appURIsForAllPossibleConstructors
          results = map (parseAppURI config . renderAppURI config) uris
      results `shouldBe` map Just uris
    it "should return Nothing for a non-URI" $ do
      let config = defaultConfig
          input = "qwe"
          r = parseAppURI config input
      r `shouldBe` Nothing
    it "should return Nothing for a neither http nor https scheme" $ do
      let config = defaultConfig
          input = "ftp://a.com/1"
          r = parseAppURI config input
      r `shouldBe` Nothing
    it "should return Nothing for an http URI if cfUseHTTPS is True" $ do
      let config = defaultConfig {cfUseHTTPS = True, cfDomain = "a.com"}
          input = "http://a.com/users/1"
          r = parseAppURI config input
      r `shouldBe` Nothing
    it "should return Nothing for an https URI if cfUseHTTPS is False" $ do
      let config = defaultConfig {cfUseHTTPS = False, cfDomain = "a.com"}
          input = "https://a.com/users/1"
          r = parseAppURI config input
      r `shouldBe` Nothing
    it "should return Nothing for a URI if the domain mismatches cfDomain" $ do
      let config =
            defaultConfig {cfUseHTTPS = False, cfDomain = "expecteddomain"}
          input = "http://otherdomain/users/1"
          r = parseAppURI config input
      r `shouldBe` Nothing
    it
      "should return True if the domain and scheme matches the config and the path is parsable" $ do
      let config = defaultConfig {cfUseHTTPS = False, cfDomain = "a.com"}
          input = "http://a.com/users/1"
          r = parseAppURI config input
      r `shouldBe` Just (UserURI $ UserId 1)
    it "should parse good URI with a different port than specified in config" $ do
      let config =
            defaultConfig
              {cfUseHTTPS = False, cfDomain = "a.com", cfPort = Just "1"}
          input = "http://a.com:2/users/1"
          r = parseAppURI config input
      r `shouldBe` Just (UserURI $ UserId 1)
    it "should parse good URI with a port although the config port is Nothing" $ do
      let config =
            defaultConfig
              {cfUseHTTPS = False, cfDomain = "a.com", cfPort = Nothing}
          input = "http://a.com:2/users/1"
          r = parseAppURI config input
      r `shouldBe` Just (UserURI $ UserId 1)
    it
      "should parse good URI without a port although the config port is specified" $ do
      let config =
            defaultConfig
              {cfUseHTTPS = False, cfDomain = "a.com", cfPort = Just "1"}
          input = "http://a.com/users/1"
          r = parseAppURI config input
      r `shouldBe` Just (UserURI $ UserId 1)

defaultAppURI :: AppURI
defaultAppURI = ImageURI $ ImageId 1

defaultConfig :: AppURIConfig
defaultConfig =
  AppURIConfig {cfUseHTTPS = False, cfDomain = "example.com", cfPort = Nothing}

appURIsForAllPossibleConstructors :: [AppURI]
appURIsForAllPossibleConstructors =
  [ ImageURI $ ImageId 1
  , UsersURI
  , UserURI $ UserId 1
  , AuthorsURI
  , AuthorURI $ AuthorId 1
  , CategoriesURI
  , CategoryURI $ CategoryId 1
  , NewsURI
  , TagsURI
  , TagURI $ TagId 1
  ]
  where
    _addNewElementToTheListAboveIfDoesNotCompile =
      \case
        ImageURI _ -> ()
        UsersURI -> ()
        UserURI _ -> ()
        AuthorsURI -> ()
        AuthorURI _ -> ()
        CategoriesURI -> ()
        CategoryURI _ -> ()
        NewsURI -> ()
        TagsURI -> ()
        TagURI _ -> ()
        DraftsURI -> ()
        DraftURI _ -> ()
