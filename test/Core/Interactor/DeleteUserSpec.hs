module Core.Interactor.DeleteUserSpec
  ( spec
  ) where

import qualified Core.Authentication as A
import Core.AuthenticationStubs
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
      let isAdmin = True
          credentials = Just stubCredentials
          uid = UserId 1
          h =
            I.Handle
              { hDeleteUser = const $ writeIORef deleteCommandIsIssued True
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission isAdmin
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
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission True
              }
      I.run h credentials uid
      readIORef passedUserId `shouldReturn` uid
    it
      "should throw NoPermissionException an exception if the actor is \
      \authenticated, but not an admin" $ do
      let uid = UserId 1
          isAdmin = False
          credentials = Just stubCredentials
          h =
            I.Handle
              { hDeleteUser = const $ pure ()
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission isAdmin
              }
      I.run h credentials uid `shouldThrow` isNoPermissionException
    it "should throw NoPermissionException for an anonymous actor" $ do
      let uid = UserId 1
          isAdmin = False
          credentials = Nothing
          h =
            I.Handle
              { hDeleteUser = const $ pure ()
              , hAuthHandle =
                  stubAuthHandleIdentifyingUserWithAdminPermission isAdmin
              }
      I.run h credentials uid `shouldThrow` isNoPermissionException

stubCredentials :: A.Credentials
stubCredentials = A.TokenCredentials (UserId 1) (A.SecretToken "")
