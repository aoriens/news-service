module Core.AuthenticationStubs
  ( stubAuthHandleIdentifyingUserWithAdminPermission
  ) where

import Core.Authentication
import qualified Logger

stubAuthHandleIdentifyingUserWithAdminPermission ::
     Applicative m => Bool -> Handle m
stubAuthHandleIdentifyingUserWithAdminPermission isAdmin =
  Handle
    { hGetUserAuthData = \_ -> pure $ Just (SecretTokenHash "", isAdmin)
    , hTokenMatchesHash = \_ _ -> True
    , hLoggerHandle = Logger.Handle $ \_ _ _ -> pure ()
    }
