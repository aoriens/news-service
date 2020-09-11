module Core.Interactor.DeleteUserSpec
  ( spec
  ) where

import qualified Core.Authentication as A
import Core.Authentication.Fakes
import Core.Exception
import qualified Core.Interactor.DeleteUser as I
import Core.User
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should issue the delete command to the gateway if the actor is admin" $ do
      deleteCommandIsIssued <- newIORef False
      let credentials = Just stubCredentials
          uid = UserId 1
          h =
            I.Handle
              { hDeleteUser = const $ writeIORef deleteCommandIsIssued True
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      I.run h credentials uid
      readIORef deleteCommandIsIssued `shouldReturn` True
    it "should pass the UserId argument to the gateway delete command" $ do
      passedUserId <- newIORef undefined
      let uid = UserId 8
          credentials = Just stubCredentials
          h =
            I.Handle
              { hDeleteUser = writeIORef passedUserId
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      I.run h credentials uid
      readIORef passedUserId `shouldReturn` uid
    it
      "should throw NoPermissionException an exception if the actor is \
      \authenticated, but not an admin" $ do
      userIsDeleted <- newIORef False
      let uid = UserId 1
          credentials = Just stubCredentials
          h =
            I.Handle
              { hDeleteUser = \_ -> writeIORef userIsDeleted True
              , hAuthHandle = stubAuthHandleReturningIdentifiedNonAdminUser
              }
      I.run h credentials uid `shouldThrow` isNoPermissionException
      readIORef userIsDeleted `shouldReturn` False
    it "should throw NoPermissionException for an anonymous actor" $ do
      userIsDeleted <- newIORef False
      let uid = UserId 1
          credentials = Nothing
          h =
            I.Handle
              { hDeleteUser = \_ -> writeIORef userIsDeleted True
              , hAuthHandle = stubAuthHandleReturningAnonymousUser
              }
      I.run h credentials uid `shouldThrow` isNoPermissionException
      readIORef userIsDeleted `shouldReturn` False

stubCredentials :: A.Credentials
stubCredentials = A.TokenCredentials (UserId 1) (A.SecretToken "")
