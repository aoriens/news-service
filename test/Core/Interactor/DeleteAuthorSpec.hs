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
    itShouldAuthorizeBeforeOperation AdminPermission $ \authUser authorizationHandle onSuccess -> do
      let authorId' = AuthorId 1
          h =
            stubHandle
              { hDeleteAuthor = \_ -> onSuccess >> pure True
              , hAuthorizationHandle = authorizationHandle
              }
      run h authUser authorId'
    it "should pass the AuthorId argument to the gateway delete command" $ do
      passedAuthorId <- newIORef undefined
      let expectedAuthorId = AuthorId 8
          h =
            stubHandle
              { hDeleteAuthor =
                  \authorId' -> writeIORef passedAuthorId authorId' >> pure True
              }
      run h someAuthUser expectedAuthorId
      readIORef passedAuthorId `shouldReturn` expectedAuthorId
    it
      "should throw RequestedEntityNotFoundException if the gateway returned False" $ do
      let authorId = AuthorId 8
          h = stubHandle {hDeleteAuthor = \_ -> pure False}
      run h someAuthUser authorId `shouldThrow`
        isRequestedEntityNotFoundException

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteAuthor = const $ pure True
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
