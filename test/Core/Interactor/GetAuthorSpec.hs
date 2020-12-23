module Core.Interactor.GetAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Exception
import Core.Interactor.GetAuthor
import Core.Permission
import Core.Stubs
import Data.IORef
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
      passedAuthorId <- newIORef Nothing
      let expectedAuthorId = AuthorId 8
          h =
            defaultHandle
              { hGetAuthor =
                  \i -> writeIORef passedAuthorId (Just i) >> pure Nothing
              }
      _ <- run h someAdminUser expectedAuthorId
      readIORef passedAuthorId `shouldReturn` Just expectedAuthorId

defaultHandle :: Handle IO
defaultHandle = Handle {hGetAuthor = const $ pure Nothing}

stubAuthorId :: AuthorId
stubAuthorId = AuthorId 1
