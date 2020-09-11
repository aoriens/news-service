module Core.Interactor.GetAuthorsSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Fakes
import Core.Author
import Core.Exception
import Core.Interactor.GetAuthors
import Core.Pagination
import Core.User
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should return authors from the gateway, if the actor is admin" $ do
      let expectedAuthors =
            [ Author
                { authorId = AuthorId 9
                , authorDescription = ""
                , authorUser =
                    User
                      { userId = UserId 12
                      , userFirstName = Nothing
                      , userLastName = ""
                      , userAvatarId = Nothing
                      , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
                      , userIsAdmin = False
                      }
                }
            ]
          h =
            defaultHandle
              { hGetAuthors = const $ pure expectedAuthors
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      authors <- run h stubCredentials noPageQuery
      authors `shouldBe` expectedAuthors
    it "should pass page got from Pagination to the gateway" $ do
      passedPage <- newIORef undefined
      let pageQuery = PageQuery (Just 1) (Just 1)
          expectedPage = Page (PageOffset 1) (PageLimit 1)
          unexpectedPage = Page (PageOffset 2) (PageLimit 2)
          h =
            defaultHandle
              { hGetAuthors = \p -> writeIORef passedPage p >> pure []
              , hPagerHandle =
                  PagerHandle $ \pq ->
                    Right $
                    if pq == pageQuery
                      then expectedPage
                      else unexpectedPage
              }
      _ <- run h stubCredentials pageQuery
      readIORef passedPage `shouldReturn` expectedPage
    it
      "should throw NoPermissionException if the actor is an identified non-admin" $ do
      let h =
            defaultHandle
              {hAuthHandle = stubAuthHandleReturningIdentifiedNonAdminUser}
      run h stubCredentials noPageQuery `shouldThrow` isNoPermissionException
    it "should throw NoPermissionException if the actor is anonymous" $ do
      let h = defaultHandle {hAuthHandle = stubAuthHandleReturningAnonymousUser}
      run h stubCredentials noPageQuery `shouldThrow` isNoPermissionException

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetAuthors = const $ pure []
    , hAuthHandle = stubAuthHandleReturningAdminUser
    , hPagerHandle = PagerHandle . const $ Right defaultPage
    }

noPageQuery :: PageQuery
noPageQuery = PageQuery Nothing Nothing

defaultPage :: Page
defaultPage = Page (PageOffset 0) (PageLimit 0)

stubCredentials :: Maybe Credentials
stubCredentials = Nothing
