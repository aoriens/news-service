module Core.Authentication.Fakes
  ( stubAuthHandleReturningAdminUser
  , stubAuthHandleReturningIdentifiedNonAdminUser
  , stubAuthHandleReturningAnonymousUser
  ) where

import Core.Authentication
import Core.User

stubAuthHandleReturningAdminUser :: Applicative m => Handle m
stubAuthHandleReturningAdminUser =
  Handle $ \_ -> pure $ IdentifiedUser (UserId 276194) True

stubAuthHandleReturningIdentifiedNonAdminUser :: Applicative m => Handle m
stubAuthHandleReturningIdentifiedNonAdminUser =
  Handle $ \_ -> pure $ IdentifiedUser (UserId 276194) False

stubAuthHandleReturningAnonymousUser :: Applicative m => Handle m
stubAuthHandleReturningAnonymousUser = Handle $ \_ -> pure AnonymousUser
