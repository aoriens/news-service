module Core.Authorization.ImplSpec
  ( spec
  ) where

import Core.Authorization
import qualified Core.Authorization.Impl as Impl
import Core.User
import Test.Hspec

spec :: Spec
spec =
  describe "hasPermission" $ do
    it "should return True if the user is an admin" $ do
      let h = Impl.new
          user = IdentifiedUser (UserId 1) True []
          r = hHasPermission h AdminPermission user
      r `shouldBe` True
    it "should return False if the user is anonymous" $ do
      let h = Impl.new
          user = AnonymousUser
          r = hHasPermission h AdminPermission user
      r `shouldBe` False
    it "should rqeturn False if the user is identified and is not an admin" $ do
      let h = Impl.new
          user = IdentifiedUser (UserId 1) False []
          r = hHasPermission h AdminPermission user
      r `shouldBe` False
