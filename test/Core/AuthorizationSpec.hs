module Core.AuthorizationSpec
  ( spec
  ) where

import Core.Authorization
import Core.Exception
import Core.User
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "requiresAdminPermission" $ do
    it "should execute the action if the user is an admin" $ do
      actionWasRun <- newIORef False
      let user = IdentifiedUser (UserId 1) True
          action = writeIORef actionWasRun True
      requiresAdminPermission user "" action
      readIORef actionWasRun `shouldReturn` True
    it "should throw NoPermissionException if the user is anonymous" $ do
      let user = AnonymousUser
      requiresAdminPermission user "" (pure ()) `shouldThrow`
        isNoPermissionException
    it
      "should throw NoPermissionException if the user is identified and is not an admin" $ do
      let user = IdentifiedUser (UserId 1) False
      requiresAdminPermission user "" (pure ()) `shouldThrow`
        isNoPermissionException

isNoPermissionException :: CoreException -> Bool
isNoPermissionException NoPermissionException {} = True
isNoPermissionException _ = False
