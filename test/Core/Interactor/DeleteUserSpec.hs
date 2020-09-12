module Core.Interactor.DeleteUserSpec
  ( spec
  ) where

import Control.Exception
import qualified Core.Authentication as A
import Core.Authentication.Fakes
import Core.Author
import Core.EntityId
import Core.Exception
import Core.Interactor.DeleteUser
import Core.Pagination
import Core.User
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should issue the delete command to the gateway if the actor is admin" $ do
      deleteCommandIsIssued <- newIORef False
      let uid = UserId 1
          h =
            stubHandle
              { hDeleteUser =
                  \_ _ ->
                    writeIORef deleteCommandIsIssued True >> pure (Right ())
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      run h stubCredentials uid
      readIORef deleteCommandIsIssued `shouldReturn` True
    it
      "should throw DependentEntitiesPreventDeletionException if \
       \the gateway returned Left DependentEntitiesPreventDeletion" $ do
      let uid = UserId 1
          authorIds = [AuthorEntityId $ AuthorId 0]
          h =
            stubHandle
              { hDeleteUser =
                  \_ _ ->
                    pure . Left $ DependentEntitiesPreventDeletion authorIds
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      r <- try $ run h stubCredentials uid
      r `shouldBe`
        Left
          (DependentEntitiesPreventDeletionException
             (UserEntityId uid)
             authorIds)
    it "should pass the UserId argument to the gateway delete command" $ do
      passedUserId <- newIORef undefined
      let expectedUid = UserId 8
          h =
            stubHandle
              { hDeleteUser =
                  \uid _ -> writeIORef passedUserId uid >> pure (Right ())
              }
      run h stubCredentials expectedUid
      readIORef passedUserId `shouldReturn` expectedUid
    it
      "should throw NoPermissionException an exception if the actor is \
      \authenticated, but not an admin" $ do
      userIsDeleted <- newIORef False
      let uid = UserId 1
          h =
            stubHandle
              { hDeleteUser =
                  \_ _ -> writeIORef userIsDeleted True >> pure (Right ())
              , hAuthHandle = stubAuthHandleReturningIdentifiedNonAdminUser
              }
      run h stubCredentials uid `shouldThrow` isNoPermissionException
      readIORef userIsDeleted `shouldReturn` False
    it "should throw NoPermissionException for an anonymous actor" $ do
      userIsDeleted <- newIORef False
      let uid = UserId 1
          h =
            stubHandle
              { hDeleteUser =
                  \_ _ -> writeIORef userIsDeleted True >> pure (Right ())
              , hAuthHandle = stubAuthHandleReturningAnonymousUser
              }
      run h stubCredentials uid `shouldThrow` isNoPermissionException
      readIORef userIsDeleted `shouldReturn` False

stubCredentials :: Maybe A.Credentials
stubCredentials = Nothing

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteUser = \_ _ -> pure (Right ())
    , hAuthHandle = stubAuthHandleReturningAdminUser
    , hDefaultEntityListRange = PageSpec (PageOffset 0) (PageLimit 0)
    }
