module Core.Interactor.CreateAuthorSpec
  ( spec
  ) where

import qualified Core.Authentication as A
import Core.Authentication.Fakes
import Core.Author
import Core.Exception
import Core.Interactor.CreateAuthor
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
      createAuthorCommandIsIssued <- newIORef False
      let isAdmin = True
          credentials = Just stubCredentials
          uid = UserId 1
          description = ""
          h =
            Handle
              { hCreateAuthor =
                  \_ _ -> do
                    writeIORef createAuthorCommandIsIssued True
                    pure $ Right stubAuthor
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission isAdmin
              }
      _ <- run h credentials uid description
      readIORef createAuthorCommandIsIssued `shouldReturn` True
    it
      "should throw NoPermissionException if the user is an identified non-admin" $ do
      let isAdmin = False
          credentials = Just stubCredentials
          uid = UserId 1
          description = ""
          h =
            Handle
              { hCreateAuthor = \_ _ -> pure $ Right stubAuthor
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission isAdmin
              }
      run h credentials uid description `shouldThrow` isNoPermissionException
    it "should throw NoPermissionException if the user is anonymous" $ do
      let credentials = Nothing
          uid = UserId 1
          description = ""
          h =
            Handle
              { hCreateAuthor = \_ _ -> pure $ Right stubAuthor
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission False
              }
      run h credentials uid description `shouldThrow` isNoPermissionException
    it "should pass userId and description data to the gateway in a normal case" $ do
      userIdAndDescription <- newIORef undefined
      let credentials = Just stubCredentials
          expectedUid = UserId 1
          expectedDescription = "q"
          h =
            Handle
              { hCreateAuthor =
                  \uid desc -> do
                    writeIORef userIdAndDescription (uid, desc)
                    pure $ Right stubAuthor
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission True
              }
      _ <- run h credentials expectedUid expectedDescription
      readIORef userIdAndDescription `shouldReturn`
        (expectedUid, expectedDescription)
    it "should return author returned from the gateway if created successfully" $ do
      let credentials = Just stubCredentials
          uid = UserId 1
          description = "q"
          expectedResult = Right stubAuthor
          h =
            Handle
              { hCreateAuthor = \_ _ -> pure expectedResult
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission True
              }
      r <- run h credentials uid description
      r `shouldBe` expectedResult
    it "should return failure returned from the gateway if any" $ do
      let credentials = Just stubCredentials
          uid = UserId 1
          description = "q"
          expectedResult = Left UnknownUserId
          h =
            Handle
              { hCreateAuthor = \_ _ -> pure expectedResult
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission True
              }
      r <- run h credentials uid description
      r `shouldBe` expectedResult

stubCredentials :: A.Credentials
stubCredentials = A.TokenCredentials (UserId 1) (A.SecretToken "")

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
