module Core.Interactor.UpdateAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Exception
import Core.Interactor.UpdateAuthor
import Core.Stubs
import Data.IORef
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it
      "should throw NoPermissionException and do not update the author if the user is not an admin" $ do
      hasUpdatedAuthor <- newIORef False
      let h =
            Handle
              { hUpdateAuthor =
                  \_ _ -> do
                    writeIORef hasUpdatedAuthor True
                    pure Nothing
              }
      run h someNonAdminUser (AuthorId 0) "" `shouldThrow`
        isNoPermissionException
      readIORef hasUpdatedAuthor `shouldReturn` False
    it
      "should pass authorId and description data to the gateway in a normal case" $ do
      authorIdAndDescription <- newIORef Nothing
      let expectedAuthorId = AuthorId 6
          expectedDescription = "q"
          h =
            Handle
              { hUpdateAuthor =
                  \aid desc -> do
                    writeIORef authorIdAndDescription $ Just (aid, desc)
                    pure $ Just stubAuthor
              }
      _ <- run h someAdminUser expectedAuthorId expectedDescription
      readIORef authorIdAndDescription `shouldReturn`
        Just (expectedAuthorId, expectedDescription)
    it "should return author returned from the gateway if updated successfully" $ do
      let aid = AuthorId 1
          description = "q"
          expectedAuthor = stubAuthor
          h = Handle {hUpdateAuthor = \_ _ -> pure $ Just expectedAuthor}
      r <- run h someAdminUser aid description
      r `shouldBe` expectedAuthor
    it
      "should throw RequestedEntityNotFoundException if the gateway returned Nothing" $ do
      let aid = AuthorId 1
          description = "q"
          h = Handle {hUpdateAuthor = \_ _ -> pure Nothing}
      run h someAdminUser aid description `shouldThrow`
        isRequestedEntityNotFoundException
