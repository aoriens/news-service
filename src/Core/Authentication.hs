module Core.Authentication
  ( authenticate
  , Handle(..)
  , Credentials(..)
  , SecretToken(..)
  , SecretTokenHash(..)
  , AuthenticatedUser(..)
  , IsAdmin(..)
  , BadCredentialsException
  ) where

import Control.Monad.Catch
import Core.User
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Logger

data Handle m =
  Handle
    { hGetUserAuthData :: UserId -> m (Maybe (SecretTokenHash, IsAdmin))
    , hTokenMatchesHash :: SecretToken -> SecretTokenHash -> Bool
    , hLoggerHandle :: Logger.Handle m
    }

data Credentials =
  TokenCredentials !UserId !SecretToken
  deriving (Eq, Show)

newtype SecretToken =
  SecretToken
    { secretTokenBytes :: BS.ByteString
    }
  deriving (Eq, Show)

newtype SecretTokenHash =
  SecretTokenHash
    { secretTokenHashBytes :: BS.ByteString
    }
  deriving (Eq, Show)

data AuthenticatedUser
  = AnonymousUser
  | IdentifiedUser !UserId !IsAdmin
  deriving (Eq, Show)

newtype IsAdmin =
  IsAdmin
    { getIsAdmin :: Bool
    }
  deriving (Eq, Show)

-- | Authenticate a user with credentials. It can throw
-- 'BadCredentialsException'.
authenticate ::
     MonadThrow m => Handle m -> Maybe Credentials -> m AuthenticatedUser
authenticate _ Nothing = pure AnonymousUser
authenticate h (Just (TokenCredentials userIdent token)) = do
  optData <- hGetUserAuthData h userIdent
  case optData of
    Nothing ->
      throwM . BadCredentialsException $
      "User does not exist: " <> T.pack (show userIdent)
    Just (hash, isAdmin)
      | hTokenMatchesHash h token hash -> do
        let authUser = IdentifiedUser userIdent isAdmin
        Logger.info (hLoggerHandle h) $
          "Authentication success: " <> T.pack (show authUser)
        pure authUser
      | otherwise -> throwM $ BadCredentialsException "secret token mismatch"

newtype BadCredentialsException =
  BadCredentialsException
    { badCredentialsExceptionReason :: T.Text
    }
  deriving (Show)

instance Exception BadCredentialsException
