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
import Core.DTO.User
import qualified Data.ByteString as BS

data Handle m =
  Handle
    { hGetUserAuthData :: UserId -> m (Maybe (SecretTokenHash, IsAdmin))
    , hTokenMatchesHash :: SecretToken -> SecretTokenHash -> Bool
    }

data Credentials =
  TokenCredentials !UserId !SecretToken

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

newtype IsAdmin =
  IsAdmin
    { getIsAdmin :: Bool
    }

-- | Authenticate a user with credentials. It can throw
-- 'BadCredentialsException'.
authenticate :: MonadThrow m => Handle m -> Maybe Credentials -> m AuthenticatedUser
authenticate _ Nothing = pure AnonymousUser
authenticate h (Just (TokenCredentials userIdent token)) = do
  optData <- hGetUserAuthData h userIdent
  case optData of
    Nothing -> throwM BadCredentialsException
    Just (hash, isAdmin)
      | hTokenMatchesHash h token hash ->
        pure $ IdentifiedUser userIdent isAdmin
      | otherwise -> throwM BadCredentialsException

data BadCredentialsException =
  BadCredentialsException
  deriving (Show)

instance Exception BadCredentialsException
