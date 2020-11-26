module Core.Authorization.Test
  ( itShouldAuthorizeBeforeOperation
  , noOpAuthorizationHandle
  ) where

import Core.Authentication
import Core.Authentication.Test
import Core.Authorization
import Core.Exception
import Core.User
import Test.AsyncExpectation
import Test.Hspec

type OnSuccess = IO ()

itShouldAuthorizeBeforeOperation ::
     HasCallStack
  => Permission
  -> (AuthenticatedUser -> AuthorizationHandle -> OnSuccess -> IO ())
  -> Spec
itShouldAuthorizeBeforeOperation expectedPerm test = do
  it "should perform action if authorization succeeds" $ do
    let authorizationH = noOpAuthorizationHandle
    shouldInvokeAtLeastOnce "authorization" $ \onSuccess ->
      test someAuthUser authorizationH onSuccess
  it
    "should throw NoPermissionException if AuthorizationHandle.hHasPermission returns False" $ do
    let authorizationH = AuthorizationHandle $ \_ _ -> False
        onSuccess = expectationFailure "Action must not succeed"
    test someAuthUser authorizationH onSuccess `shouldThrow`
      isNoPermissionException
  it "should pass the given AuthenticatedUser to AuthorizationHandle" $ do
    let expectedUser = IdentifiedUser (UserId 1872134) False []
        authorizationHandle = AuthorizationHandle $ const (== expectedUser)
    test expectedUser authorizationHandle (pure ())
  it "should pass a correct permission to AuthorizationHandle" $ do
    let authorizationHandle =
          AuthorizationHandle $ \perm _ -> perm == expectedPerm
    test someAuthUser authorizationHandle (pure ())

noOpAuthorizationHandle :: AuthorizationHandle
noOpAuthorizationHandle = AuthorizationHandle $ \_ _ -> True
