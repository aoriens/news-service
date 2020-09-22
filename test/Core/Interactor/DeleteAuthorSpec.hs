module Core.Interactor.DeleteAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Authorization.Test
import Core.Exception
import Core.Interactor.DeleteAuthor
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldAuthenticateBeforeOperation $ \credentials authHandle onSuccess -> do
      let authorIdent = AuthorId 1
          h =
            stubHandle
              { hDeleteAuthor = \_ -> onSuccess >> pure True
              , hAuthHandle = authHandle
              }
      run h credentials authorIdent
    it "should pass the AuthorId argument to the gateway delete command" $ do
      passedAuthorId <- newIORef undefined
      let expectedAuthorId = AuthorId 8
          h =
            stubHandle
              { hDeleteAuthor =
                  \authorIdent ->
                    writeIORef passedAuthorId authorIdent >> pure True
              }
      run h noCredentials expectedAuthorId
      readIORef passedAuthorId `shouldReturn` expectedAuthorId
    it "should throw EntityNotFoundException if the gateway returned False" $ do
      let authorId = AuthorId 8
          h = stubHandle {hDeleteAuthor = \_ -> pure False}
      run h noCredentials authorId `shouldThrow` isEntityNotFoundException

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteAuthor = const $ pure True
    , hAuthHandle = noOpAuthenticationHandle
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
