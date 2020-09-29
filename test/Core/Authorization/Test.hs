module Core.Authorization.Test
  ( itShouldAuthenticateAndAuthorizeBeforeOperation
  , itShouldAuthorizeBeforeOperation
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

itShouldAuthenticateAndAuthorizeBeforeOperation ::
     HasCallStack
  => Permission
  -> (Maybe Credentials -> AuthenticationHandle IO -> AuthorizationHandle -> OnSuccess -> IO ())
  -> Spec
itShouldAuthenticateAndAuthorizeBeforeOperation expectedPerm test = do
  itShouldAuthenticateBeforeOperation $ \creds authenticationH ->
    test creds authenticationH noOpAuthorizationHandle
  itShouldAuthorizeBeforeOperation expectedPerm $ test Nothing

itShouldAuthorizeBeforeOperation ::
     HasCallStack
  => Permission
  -> (AuthenticationHandle IO -> AuthorizationHandle -> OnSuccess -> IO ())
  -> Spec
itShouldAuthorizeBeforeOperation expectedPerm test = do
  it "should perform action if authorization succeeds" $ do
    let authenticationH = noOpAuthenticationHandle
        authorizationH = noOpAuthorizationHandle
    shouldInvokeOnce "authorization" $ \onSuccess ->
      test authenticationH authorizationH onSuccess
  it
    "should throw NoPermissionException if AuthorizationHandle.hHasPermission returns False" $ do
    let authenticationH = noOpAuthenticationHandle
        authorizationH = AuthorizationHandle $ \_ _ -> False
        onSuccess = expectationFailure "Action must not succeed"
    test authenticationH authorizationH onSuccess `shouldThrow`
      isNoPermissionException
  it
    "should pass AuthenticatedUser from AuthenticationHandle to AuthorizationHandle" $ do
    let expectedUser = IdentifiedUser (UserId 1872134) False
        authenticationHandle = AuthenticationHandle $ \_ -> pure expectedUser
        authorizationHandle = AuthorizationHandle $ const (== expectedUser)
    test authenticationHandle authorizationHandle (pure ())
  it "should pass a correct permission to AuthorizationHandle" $ do
    let authenticationHandle = noOpAuthenticationHandle
        authorizationHandle =
          AuthorizationHandle $ \perm _ -> perm == expectedPerm
    test authenticationHandle authorizationHandle (pure ())

noOpAuthorizationHandle :: AuthorizationHandle
noOpAuthorizationHandle = AuthorizationHandle $ \_ _ -> True
