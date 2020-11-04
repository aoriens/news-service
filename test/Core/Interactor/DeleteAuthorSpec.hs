module Core.Interactor.DeleteAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
import Core.Exception
import Core.Interactor.DeleteAuthor
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldAuthenticateAndAuthorizeBeforeOperation AdminPermission $ \credentials authenticationHandle authorizationHandle onSuccess -> do
      let authorId' = AuthorId 1
          h =
            stubHandle
              { hDeleteAuthor = \_ -> onSuccess >> pure True
              , hAuthenticationHandle = authenticationHandle
              , hAuthorizationHandle = authorizationHandle
              }
      run h credentials authorId'
    it "should pass the AuthorId argument to the gateway delete command" $ do
      passedAuthorId <- newIORef undefined
      let expectedAuthorId = AuthorId 8
          h =
            stubHandle
              { hDeleteAuthor =
                  \authorId' -> writeIORef passedAuthorId authorId' >> pure True
              }
      run h noCredentials expectedAuthorId
      readIORef passedAuthorId `shouldReturn` expectedAuthorId
    it
      "should throw RequestedEntityNotFoundException if the gateway returned False" $ do
      let authorId = AuthorId 8
          h = stubHandle {hDeleteAuthor = \_ -> pure False}
      run h noCredentials authorId `shouldThrow`
        isRequestedEntityNotFoundException

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteAuthor = const $ pure True
    , hAuthenticationHandle = noOpAuthenticationHandle
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
