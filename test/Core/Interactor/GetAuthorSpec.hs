module Core.Interactor.GetAuthorSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Fakes
import Core.Author
import Core.Exception
import Core.Interactor.GetAuthor
import Core.User
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should return gateway output if the actor is admin" $ do
      let expectedAuthor =
            Just
              Author
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
          h =
            defaultHandle
              { hGetAuthor = const $ pure expectedAuthor
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      author <- run h stubCredentials stubAuthorId
      author `shouldBe` expectedAuthor
    it "should pass author id to the gateway" $ do
      passedAuthorId <- newIORef undefined
      let expectedAuthorId = AuthorId 8
          h =
            defaultHandle
              {hGetAuthor = \i -> writeIORef passedAuthorId i >> pure Nothing}
      _ <- run h stubCredentials expectedAuthorId
      readIORef passedAuthorId `shouldReturn` expectedAuthorId
    it
      "should throw NoPermissionException if the actor is an identified non-admin" $ do
      let h =
            defaultHandle
              {hAuthHandle = stubAuthHandleReturningIdentifiedNonAdminUser}
      run h stubCredentials stubAuthorId `shouldThrow` isNoPermissionException
    it "should throw NoPermissionException if the actor is anonymous" $ do
      let h = defaultHandle {hAuthHandle = stubAuthHandleReturningAnonymousUser}
      run h stubCredentials stubAuthorId `shouldThrow` isNoPermissionException

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetAuthor = const $ pure Nothing
    , hAuthHandle = stubAuthHandleReturningAdminUser
    }

stubCredentials :: Maybe Credentials
stubCredentials = Nothing

stubAuthorId :: AuthorId
stubAuthorId = AuthorId 1
