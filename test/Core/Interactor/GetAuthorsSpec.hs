module Core.Interactor.GetAuthorsSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
import Core.Interactor.GetAuthors
import Core.Pagination
import Core.Pagination.Test
import Core.User
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldAuthenticateAndAuthorizeBeforeOperation AdminPermission $ \credentials authenticationHandle authorizationHandle onSuccess -> do
      let h =
            defaultHandle
              { hGetAuthors = \_ -> onSuccess >> pure [stubAuthor]
              , hAuthenticationHandle = authenticationHandle
              , hAuthorizationHandle = authorizationHandle
              }
      void $ run h credentials noPageQuery
    it "should return authors from the gateway, if the actor is admin" $ do
      let expectedAuthors = [stubAuthor {authorId = AuthorId 9}]
          h = defaultHandle {hGetAuthors = const $ pure expectedAuthors}
      authors <- run h noCredentials noPageQuery
      authors `shouldBe` expectedAuthors
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            defaultHandle
              { hGetAuthors = \pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ run h noCredentials pageSpecQuery

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetAuthors = const $ pure []
    , hAuthenticationHandle = noOpAuthenticationHandle
    , hPageSpecParserHandle = PageSpecParserHandle . const $ Right defaultPage
    , hAuthorizationHandle = noOpAuthorizationHandle
    }

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

defaultPage :: PageSpec
defaultPage = PageSpec (PageOffset 0) (PageLimit 0)

stubAuthor :: Author
stubAuthor =
  Author
    { authorId = AuthorId 1
    , authorDescription = ""
    , authorUser =
        User
          { userId = UserId 1
          , userFirstName = Nothing
          , userLastName = ""
          , userAvatarId = Nothing
          , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
          , userIsAdmin = False
          }
    }
