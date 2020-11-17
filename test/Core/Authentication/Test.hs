module Core.Authentication.Test
  ( noCredentials
  , anyAuthUser
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

anyAuthUser :: AuthenticatedUser
anyAuthUser = AnonymousUser
