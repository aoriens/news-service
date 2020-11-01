module Core.Authorization.ImplSpec
  ( spec
  ) where

import Core.Author
import Core.Authorization
import qualified Core.Authorization.Impl as Impl
import Core.User
import Test.Hspec

spec :: Spec
spec =
  describe "hasPermission" $ do
    it "should return True for AdminPermission if the user is an admin" $ do
      let h = Impl.new
          user = IdentifiedUser (UserId 1) True []
          r = hHasPermission h AdminPermission user
      r `shouldBe` True
    it "should return False for AdminPermission if the user is anonymous" $ do
      let h = Impl.new
          user = AnonymousUser
          r = hHasPermission h AdminPermission user
      r `shouldBe` False
    it
      "should return False for AdminPermission if the user is identified and is not an admin" $ do
      let h = Impl.new
          user = IdentifiedUser (UserId 1) False []
          r = hHasPermission h AdminPermission user
      r `shouldBe` False
    it "should return False for AuthorshipPermission if the user has no authors" $ do
      let h = Impl.new
          user = IdentifiedUser (UserId 1) False []
          perm = AuthorshipPermission $ AuthorId 1
          r = hHasPermission h perm user
      r `shouldBe` False
    it
      "should return False for AuthorshipPermission if the user does not have the required author" $ do
      let h = Impl.new
          requiredAuthorId = AuthorId 1
          otherAuthorId = AuthorId 2
          user = IdentifiedUser (UserId 1) False [otherAuthorId]
          perm = AuthorshipPermission requiredAuthorId
          r = hHasPermission h perm user
      r `shouldBe` False
    it "should return False for AuthorshipPermission if the user is anonymous" $ do
      let h = Impl.new
          user = AnonymousUser
          perm = AuthorshipPermission $ AuthorId 1
          r = hHasPermission h perm user
      r `shouldBe` False
    it
      "should return True for AuthorshipPermission if the user has the specified AuthorId" $ do
      let h = Impl.new
          requiredAuthorId = AuthorId 2
          otherAuthorId = AuthorId 1
          user =
            IdentifiedUser (UserId 1) False [otherAuthorId, requiredAuthorId]
          perm = AuthorshipPermission requiredAuthorId
          r = hHasPermission h perm user
      r `shouldBe` True
