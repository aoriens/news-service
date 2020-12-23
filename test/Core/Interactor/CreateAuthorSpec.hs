module Core.Interactor.CreateAuthorSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Authorization
import Core.Authorization.Test
import Core.Interactor.CreateAuthor
import Core.Stubs
import Core.User
import Data.IORef
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
      userIdAndDescription <- newIORef Nothing
      let expectedUid = UserId 1
          expectedDescription = "q"
          h =
            stubHandle
              { hCreateAuthor =
                  \uid desc -> do
                    writeIORef userIdAndDescription $ Just (uid, desc)
                    pure $ Right stubAuthor
              }
      _ <- run h someAuthUser expectedUid expectedDescription
      readIORef userIdAndDescription `shouldReturn`
        Just (expectedUid, expectedDescription)
    it "should return author returned from the gateway if created successfully" $ do
      let uid = UserId 1
          description = "q"
          expectedResult = Right stubAuthor
          h = stubHandle {hCreateAuthor = \_ _ -> pure expectedResult}
      r <- run h someAuthUser uid description
      r `shouldBe` expectedResult
    it "should return failure returned from the gateway if any" $ do
      let uid = UserId 1
          description = "q"
          expectedResult = Left UnknownUserId
          h = stubHandle {hCreateAuthor = \_ _ -> pure expectedResult}
      r <- run h someAuthUser uid description
      r `shouldBe` expectedResult

stubHandle :: Handle IO
stubHandle =
  Handle
    { hCreateAuthor = \_ _ -> pure (Right stubAuthor)
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
