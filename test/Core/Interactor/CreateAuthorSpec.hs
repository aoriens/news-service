module Core.Interactor.CreateAuthorSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
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
    itShouldAuthorizeBeforeOperation AdminPermission $ \authUser authorizationHandle onSuccess -> do
      let uid = UserId 1
          description = ""
          h =
            stubHandle
              { hCreateAuthor = \_ _ -> onSuccess >> pure (Right stubAuthor)
              , hAuthorizationHandle = authorizationHandle
              }
      void $ run h authUser uid description
    it "should pass userId and description data to the gateway in a normal case" $ do
      userIdAndDescription <- newIORef undefined
      let expectedUid = UserId 1
          expectedDescription = "q"
          h =
            stubHandle
              { hCreateAuthor =
                  \uid desc -> do
                    writeIORef userIdAndDescription (uid, desc)
                    pure $ Right stubAuthor
              }
      _ <- run h anyAuthenticatedUser expectedUid expectedDescription
      readIORef userIdAndDescription `shouldReturn`
        (expectedUid, expectedDescription)
    it "should return author returned from the gateway if created successfully" $ do
      let uid = UserId 1
          description = "q"
          expectedResult = Right stubAuthor
          h = stubHandle {hCreateAuthor = \_ _ -> pure expectedResult}
      r <- run h anyAuthenticatedUser uid description
      r `shouldBe` expectedResult
    it "should return failure returned from the gateway if any" $ do
      let uid = UserId 1
          description = "q"
          expectedResult = Left UnknownUserId
          h = stubHandle {hCreateAuthor = \_ _ -> pure expectedResult}
      r <- run h anyAuthenticatedUser uid description
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

stubHandle :: Handle IO
stubHandle =
  Handle
    {hCreateAuthor = undefined, hAuthorizationHandle = noOpAuthorizationHandle}
