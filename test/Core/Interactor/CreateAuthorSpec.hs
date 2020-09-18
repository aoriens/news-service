module Core.Interactor.CreateAuthorSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
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
    itShouldRequireAdminPermission $ \credentials authHandle onSuccess -> do
      let uid = UserId 1
          description = ""
          h =
            Handle
              { hCreateAuthor = \_ _ -> onSuccess >> pure (Right stubAuthor)
              , hAuthHandle = authHandle
              }
      void $ run h credentials uid description
    it "should pass userId and description data to the gateway in a normal case" $ do
      userIdAndDescription <- newIORef undefined
      let expectedUid = UserId 1
          expectedDescription = "q"
          h =
            Handle
              { hCreateAuthor =
                  \uid desc -> do
                    writeIORef userIdAndDescription (uid, desc)
                    pure $ Right stubAuthor
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      _ <- run h noCredentials expectedUid expectedDescription
      readIORef userIdAndDescription `shouldReturn`
        (expectedUid, expectedDescription)
    it "should return author returned from the gateway if created successfully" $ do
      let uid = UserId 1
          description = "q"
          expectedResult = Right stubAuthor
          h =
            Handle
              { hCreateAuthor = \_ _ -> pure expectedResult
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      r <- run h noCredentials uid description
      r `shouldBe` expectedResult
    it "should return failure returned from the gateway if any" $ do
      let uid = UserId 1
          description = "q"
          expectedResult = Left UnknownUserId
          h =
            Handle
              { hCreateAuthor = \_ _ -> pure expectedResult
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      r <- run h noCredentials uid description
      r `shouldBe` expectedResult

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
