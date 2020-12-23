module Core.Interactor.UpdateAuthorSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
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
    itShouldAuthorizeBeforeOperation AdminPermission $ \authUser authorizationHandle onSuccess -> do
      let aid = AuthorId 1
          description = ""
          h =
            stubHandle
              { hUpdateAuthor =
                  \_ _ -> do
                    onSuccess
                    pure $ Just stubAuthor
              , hAuthorizationHandle = authorizationHandle
              }
      void $ run h authUser aid description
    it
      "should pass authorId and description data to the gateway in a normal case" $ do
      authorIdAndDescription <- newIORef Nothing
      let expectedAuthorId = AuthorId 6
          expectedDescription = "q"
          h =
            stubHandle
              { hUpdateAuthor =
                  \aid desc -> do
                    writeIORef authorIdAndDescription $ Just (aid, desc)
                    pure $ Just stubAuthor
              }
      _ <- run h someAuthUser expectedAuthorId expectedDescription
      readIORef authorIdAndDescription `shouldReturn`
        Just (expectedAuthorId, expectedDescription)
    it "should return author returned from the gateway if updated successfully" $ do
      let aid = AuthorId 1
          description = "q"
          expectedAuthor = stubAuthor
          h = stubHandle {hUpdateAuthor = \_ _ -> pure $ Just expectedAuthor}
      r <- run h someAuthUser aid description
      r `shouldBe` expectedAuthor
    it
      "should throw RequestedEntityNotFoundException if the gateway returned Nothing" $ do
      let aid = AuthorId 1
          description = "q"
          h = stubHandle {hUpdateAuthor = \_ _ -> pure Nothing}
      run h someAuthUser aid description `shouldThrow`
        isRequestedEntityNotFoundException

stubHandle :: Handle IO
stubHandle =
  Handle
    { hUpdateAuthor = \_ _ -> pure $ Just stubAuthor {authorId = AuthorId 99993}
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
