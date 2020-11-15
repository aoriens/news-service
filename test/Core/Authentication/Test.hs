module Core.Authentication.Test
  ( noCredentials
  , anyAuthenticatedUser
  , noOpAuthenticationHandle
  , authenticationHandleReturningIdentifiedUser
  ) where

import Core.Authentication
import Core.User

noOpAuthenticationHandle :: AuthenticationHandle IO
noOpAuthenticationHandle = AuthenticationHandle $ \_ -> pure AnonymousUser

authenticationHandleReturningIdentifiedUser :: AuthenticationHandle IO
authenticationHandleReturningIdentifiedUser =
  AuthenticationHandle $ \_ -> pure $ IdentifiedUser (UserId 1) False []

noCredentials :: Maybe Credentials
noCredentials = Nothing

anyAuthenticatedUser :: AuthenticatedUser
anyAuthenticatedUser = AnonymousUser
