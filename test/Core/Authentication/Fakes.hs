module Core.Authentication.Fakes
  ( stubAuthHandleIdentifyingUserWithAdminPermission
  ) where

import Core.Authentication
import Core.User

stubAuthHandleIdentifyingUserWithAdminPermission ::
     Applicative m => Bool -> Handle m
stubAuthHandleIdentifyingUserWithAdminPermission isAdmin =
  Handle $ \_ -> pure $ IdentifiedUser (UserId 276194) isAdmin
