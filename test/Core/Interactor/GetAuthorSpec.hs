module Core.Interactor.GetAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Interactor.GetAuthor
import Core.Permission
import Core.User
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should throw NoPermissionException if the user is not an admin" $ do
      run defaultHandle someNonAdminUser stubAuthorId `shouldThrow`
        isNoPermissionExceptionWithPermission AdminPermission
    it "should return gateway output if the actor is admin" $ do
      let expectedAuthor = Just stubAuthor {authorId = AuthorId 9}
          h = defaultHandle {hGetAuthor = const $ pure expectedAuthor}
      author <- run h someAdminUser stubAuthorId
      author `shouldBe` expectedAuthor
    it "should pass author id to the gateway" $ do
      passedAuthorId <- newIORef undefined
      let expectedAuthorId = AuthorId 8
          h =
            defaultHandle
              {hGetAuthor = \i -> writeIORef passedAuthorId i >> pure Nothing}
      _ <- run h someAdminUser expectedAuthorId
      readIORef passedAuthorId `shouldReturn` expectedAuthorId

defaultHandle :: Handle IO
defaultHandle = Handle {hGetAuthor = const $ pure Nothing}

stubAuthorId :: AuthorId
stubAuthorId = AuthorId 1

stubAuthor :: Author
stubAuthor =
  Author
    { authorId = AuthorId 1
    , authorDescription = ""
    , authorUser =
        Existing
          User
            { userId = UserId 1
            , userFirstName = Nothing
            , userLastName = ""
            , userAvatarId = Nothing
            , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
            , userIsAdmin = False
            }
    }
