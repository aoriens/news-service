module Core.Authentication.Impl
  ( Handle(..)
  , UserAuthData(..)
  , new
  ) where

import Control.Monad.Catch
import Core.Authentication hiding (authenticate)
import Core.Exception
import Core.User
import qualified Data.Text as T
import qualified Logger

data Handle m =
  Handle
    { hGetUserAuthData :: UserId -> m (Maybe UserAuthData)
    , hTokenMatchesHash :: SecretToken -> SecretTokenHash -> Bool
    , hLoggerHandle :: Logger.Handle m
    }

data UserAuthData =
  UserAuthData
    { authDataSecretTokenHash :: !SecretTokenHash
    , authDataIsAdmin :: !Bool
    }

new :: Handle m -> AuthenticationHandle m
new h = AuthenticationHandle $ authenticate h

authenticate ::
     MonadThrow m => Handle m -> Maybe Credentials -> m AuthenticatedUser
authenticate _ Nothing = pure AnonymousUser
authenticate h (Just (TokenCredentials userIdent token)) = do
  optData <- hGetUserAuthData h userIdent
  case optData of
    Nothing ->
      throwM . BadCredentialsException $
      "User does not exist: " <> T.pack (show userIdent)
    Just UserAuthData {..}
      | hTokenMatchesHash h token authDataSecretTokenHash -> do
        let authUser = IdentifiedUser userIdent authDataIsAdmin
        Logger.info (hLoggerHandle h) $
          "Authentication success: " <> T.pack (show authUser)
        pure authUser
      | otherwise -> throwM $ BadCredentialsException "secret token mismatch"
