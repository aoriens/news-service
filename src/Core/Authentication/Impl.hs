module Core.Authentication.Impl
  ( Handle(..)
  , new
  ) where

import Control.Monad.Catch
import qualified Core.Authentication as A
import Core.Exception
import Core.User
import qualified Data.Text as T
import qualified Logger

data Handle m =
  Handle
    { hGetUserAuthData :: UserId -> m (Maybe (A.SecretTokenHash, A.IsAdmin))
    , hTokenMatchesHash :: A.SecretToken -> A.SecretTokenHash -> Bool
    , hLoggerHandle :: Logger.Handle m
    }

new :: Handle m -> A.Handle m
new h = A.Handle $ authenticate h

authenticate ::
     MonadThrow m => Handle m -> Maybe A.Credentials -> m A.AuthenticatedUser
authenticate _ Nothing = pure A.AnonymousUser
authenticate h (Just (A.TokenCredentials userIdent token)) = do
  optData <- hGetUserAuthData h userIdent
  case optData of
    Nothing ->
      throwM . BadCredentialsException $
      "User does not exist: " <> T.pack (show userIdent)
    Just (hash, isAdmin)
      | hTokenMatchesHash h token hash -> do
        let authUser = A.IdentifiedUser userIdent isAdmin
        Logger.info (hLoggerHandle h) $
          "Authentication success: " <> T.pack (show authUser)
        pure authUser
      | otherwise -> throwM $ BadCredentialsException "secret token mismatch"
