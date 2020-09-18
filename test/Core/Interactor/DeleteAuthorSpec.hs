module Core.Interactor.DeleteAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Interactor.DeleteAuthor
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldRequireAdminPermission $ \credentials authHandle onSuccess -> do
      let authorIdent = AuthorId 1
          h =
            stubHandle
              { hDeleteAuthor = \_ -> onSuccess >> pure ()
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
                    writeIORef passedAuthorId authorIdent >> pure ()
              }
      run h noCredentials expectedAuthorId
      readIORef passedAuthorId `shouldReturn` expectedAuthorId

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteAuthor = const $ pure ()
    , hAuthHandle = stubAuthHandleReturningAdminUser
    }
