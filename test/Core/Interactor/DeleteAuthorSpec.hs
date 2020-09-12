module Core.Interactor.DeleteAuthorSpec
  ( spec
  ) where

import qualified Core.Authentication as A
import Core.Authentication.Fakes
import Core.Author
import Core.Exception
import Core.Interactor.DeleteAuthor
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should issue the delete command to the gateway if the actor is admin" $ do
      deleteCommandIsIssued <- newIORef False
      let authorIdent = AuthorId 1
          h =
            stubHandle
              { hDeleteAuthor =
                  \_ -> writeIORef deleteCommandIsIssued True >> pure ()
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      run h stubCredentials authorIdent
      readIORef deleteCommandIsIssued `shouldReturn` True
    it "should pass the AuthorId argument to the gateway delete command" $ do
      passedAuthorId <- newIORef undefined
      let expectedAuthorId = AuthorId 8
          h =
            stubHandle
              { hDeleteAuthor =
                  \authorIdent ->
                    writeIORef passedAuthorId authorIdent >> pure ()
              }
      run h stubCredentials expectedAuthorId
      readIORef passedAuthorId `shouldReturn` expectedAuthorId
    it
      "should throw NoPermissionException an exception if the actor is \
      \authenticated, but not an admin" $ do
      authorIsDeleted <- newIORef False
      let authorIdent = AuthorId 1
          h =
            stubHandle
              { hDeleteAuthor = \_ -> writeIORef authorIsDeleted True >> pure ()
              , hAuthHandle = stubAuthHandleReturningIdentifiedNonAdminUser
              }
      run h stubCredentials authorIdent `shouldThrow` isNoPermissionException
      readIORef authorIsDeleted `shouldReturn` False
    it "should throw NoPermissionException for an anonymous actor" $ do
      authorIsDeleted <- newIORef False
      let authorIdent = AuthorId 1
          h =
            stubHandle
              { hDeleteAuthor = \_ -> writeIORef authorIsDeleted True >> pure ()
              , hAuthHandle = stubAuthHandleReturningAnonymousUser
              }
      run h stubCredentials authorIdent `shouldThrow` isNoPermissionException
      readIORef authorIsDeleted `shouldReturn` False

stubCredentials :: Maybe A.Credentials
stubCredentials = Nothing

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteAuthor = const $ pure ()
    , hAuthHandle = stubAuthHandleReturningAdminUser
    }
