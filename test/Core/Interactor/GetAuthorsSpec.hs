module Core.Interactor.GetAuthorsSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import qualified Core.Authorization.Impl
import Core.Interactor.GetAuthors
import Core.Pagination
import Core.User
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldRequireAdminPermission $ \credentials authHandle onSuccess -> do
      let h =
            defaultHandle
              { hGetAuthors = \_ -> onSuccess >> pure [stubAuthor]
              , hAuthHandle = authHandle
              }
      void $ run h credentials noPageQuery
    it "should return authors from the gateway, if the actor is admin" $ do
      let expectedAuthors = [stubAuthor {authorId = AuthorId 9}]
          h =
            defaultHandle
              { hGetAuthors = const $ pure expectedAuthors
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      authors <- run h noCredentials noPageQuery
      authors `shouldBe` expectedAuthors
    it "should pass page got from Pagination to the gateway" $ do
      passedPage <- newIORef undefined
      let pageQuery = PageSpecQuery (Just 1) (Just 1)
          expectedPage = PageSpec (PageOffset 1) (PageLimit 1)
          unexpectedPage = PageSpec (PageOffset 2) (PageLimit 2)
          h =
            defaultHandle
              { hGetAuthors = \p -> writeIORef passedPage p >> pure []
              , hPageSpecParserHandle =
                  PageSpecParserHandle $ \pq ->
                    Right $
                    if pq == pageQuery
                      then expectedPage
                      else unexpectedPage
              }
      _ <- run h noCredentials pageQuery
      readIORef passedPage `shouldReturn` expectedPage

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetAuthors = const $ pure []
    , hAuthHandle = stubAuthHandleReturningAdminUser
    , hPageSpecParserHandle = PageSpecParserHandle . const $ Right defaultPage
    , hAuthorizationHandle = Core.Authorization.Impl.new
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
