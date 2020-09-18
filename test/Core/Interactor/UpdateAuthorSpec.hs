module Core.Interactor.UpdateAuthorSpec
  ( spec
  ) where

import qualified Core.Authentication as A
import Core.Authentication.Fakes
import Core.Author
import Core.Exception
import Core.Interactor.UpdateAuthor
import Core.User
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it
      "should pass control to the gateway if the actor is administrator and the user exists" $ do
      updateAuthorCommandIsIssued <- newIORef False
      let aid = AuthorId 1
          description = ""
          h =
            Handle
              { hUpdateAuthor =
                  \_ _ -> do
                    writeIORef updateAuthorCommandIsIssued True
                    pure $ Just stubAuthor
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      _ <- run h stubCredentials aid description
      readIORef updateAuthorCommandIsIssued `shouldReturn` True
    it
      "should throw NoPermissionException if the user is an identified non-admin" $ do
      let aid = AuthorId 1
          description = ""
          h =
            stubHandle
              {hAuthHandle = stubAuthHandleReturningIdentifiedNonAdminUser}
      run h stubCredentials aid description `shouldThrow`
        isNoPermissionException
    it "should throw NoPermissionException if the user is anonymous" $ do
      let credentials = Nothing
          aid = AuthorId 1
          description = ""
          h = stubHandle {hAuthHandle = stubAuthHandleReturningAnonymousUser}
      run h credentials aid description `shouldThrow` isNoPermissionException
    it
      "should pass authorId and description data to the gateway in a normal case" $ do
      authorIdAndDescription <- newIORef undefined
      let expectedAuthorId = AuthorId 6
          expectedDescription = "q"
          h =
            stubHandle
              { hUpdateAuthor =
                  \aid desc -> do
                    writeIORef authorIdAndDescription (aid, desc)
                    pure $ Just stubAuthor
              }
      _ <- run h stubCredentials expectedAuthorId expectedDescription
      readIORef authorIdAndDescription `shouldReturn`
        (expectedAuthorId, expectedDescription)
    it "should return author returned from the gateway if updated successfully" $ do
      let aid = AuthorId 1
          description = "q"
          expectedAuthor = stubAuthor
          h = stubHandle {hUpdateAuthor = \_ _ -> pure $ Just expectedAuthor}
      r <- run h stubCredentials aid description
      r `shouldBe` expectedAuthor
    it "should throw EntityNotFoundException if the gateway returned Nothing" $ do
      let aid = AuthorId 1
          description = "q"
          h = stubHandle {hUpdateAuthor = \_ _ -> pure Nothing}
      run h stubCredentials aid description `shouldThrow`
        isEntityNotFoundException

stubHandle :: Handle IO
stubHandle =
  Handle
    { hUpdateAuthor = \_ _ -> pure $ Just stubAuthor {authorId = AuthorId 99993}
    , hAuthHandle = stubAuthHandleReturningAdminUser
    }

stubCredentials :: Maybe A.Credentials
stubCredentials = Nothing

stubAuthor :: Author
stubAuthor =
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
