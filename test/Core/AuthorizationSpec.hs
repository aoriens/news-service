module Core.AuthorizationSpec
  ( spec
  ) where

import Core.Authorization
import Core.User
import Test.Hspec

spec :: Spec
spec =
  describe "hasAdminPermission" $ do
    it "should return True if the user is an admin" $ do
      let user = IdentifiedUser (UserId 1) True
          r = hasAdminPermission user
      r `shouldBe` True
    it "should return False if the user is anonymous" $ do
      let user = AnonymousUser
          r = hasAdminPermission user
      r `shouldBe` False
    it "should return False if the user is identified and is not an admin" $ do
      let user = IdentifiedUser (UserId 1) False
          r = hasAdminPermission user
      r `shouldBe` False
