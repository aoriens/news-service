module Core.Authentication.Test
  ( noCredentials
  , someAuthUser
  , someIdentifiedAuthUser
  , someAdminUser
  , someNonAdminUser
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

someAuthUser :: AuthenticatedUser
someAuthUser = AnonymousUser

someIdentifiedAuthUser :: AuthenticatedUser
someIdentifiedAuthUser = IdentifiedUser (UserId (-1)) False []

someAdminUser :: AuthenticatedUser
someAdminUser = IdentifiedUser (UserId 0) True []

someNonAdminUser :: AuthenticatedUser
someNonAdminUser = IdentifiedUser (UserId 0) False []
