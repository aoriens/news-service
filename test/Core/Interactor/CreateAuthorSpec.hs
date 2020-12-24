module Core.Interactor.CreateAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Exception
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
    it
      "should throw NoPermissionException and should not invoke the gateway if the user is not an admin" $ do
      hasCreatedUser <- newIORef False
      let h =
            Handle
              { hCreateAuthor =
                  \_ _ ->
                    writeIORef hasCreatedUser True >> pure (Right stubAuthor)
              }
      run h someNonAdminUser (UserId 0) "" `shouldThrow` isNoPermissionException
      readIORef hasCreatedUser `shouldReturn` False
    it
      "should pass userId and description data to the gateway if the user is admin" $ do
      userIdAndDescription <- newIORef Nothing
      let expectedUid = UserId 1
          expectedDescription = "q"
          h =
            Handle
              { hCreateAuthor =
                  \uid desc -> do
                    writeIORef userIdAndDescription $ Just (uid, desc)
                    pure $ Right stubAuthor
              }
      _ <- run h someAdminUser expectedUid expectedDescription
      readIORef userIdAndDescription `shouldReturn`
        Just (expectedUid, expectedDescription)
    it "should return author returned from the gateway if created successfully" $ do
      let uid = UserId 1
          description = "q"
          expectedResult = Right stubAuthor
          h = Handle {hCreateAuthor = \_ _ -> pure expectedResult}
      r <- run h someAdminUser uid description
      r `shouldBe` expectedResult
    it "should return failure returned from the gateway if any" $ do
      let uid = UserId 1
          description = "q"
          expectedResult = Left UnknownUserId
          h = Handle {hCreateAuthor = \_ _ -> pure expectedResult}
      r <- run h someAdminUser uid description
      r `shouldBe` expectedResult
