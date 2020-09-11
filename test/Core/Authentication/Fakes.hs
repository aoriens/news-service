module Core.Authentication.Fakes
  ( stubAuthHandleReturningAdminUser
  , stubAuthHandleReturningIdentifiedNonAdminUser
  , stubAuthHandleReturningAnonymousUser
  ) where

import Core.Authentication
import Core.User

stubAuthHandleReturningAdminUser :: Applicative m => AuthenticationHandle m
stubAuthHandleReturningAdminUser =
  AuthenticationHandle $ \_ -> pure $ IdentifiedUser (UserId 276194) True

stubAuthHandleReturningIdentifiedNonAdminUser ::
     Applicative m => AuthenticationHandle m
stubAuthHandleReturningIdentifiedNonAdminUser =
  AuthenticationHandle $ \_ -> pure $ IdentifiedUser (UserId 276194) False

stubAuthHandleReturningAnonymousUser :: Applicative m => AuthenticationHandle m
stubAuthHandleReturningAnonymousUser =
  AuthenticationHandle $ \_ -> pure AnonymousUser
