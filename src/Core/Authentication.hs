{-# LANGUAGE RankNTypes #-}

module Core.Authentication
  ( Handle(..)
  , Credentials(..)
  , SecretToken(..)
  , SecretTokenHash(..)
  , AuthenticatedUser(..)
  , IsAdmin
  ) where

import Control.Monad.Catch
import Core.User
import qualified Data.ByteString as B

newtype Handle m =
  Handle
    { authenticate :: MonadThrow m =>
                        Maybe Credentials -> m AuthenticatedUser
      -- ^ Authenticate a user with credentials. It can throw
      -- 'CoreException'.
    }

data Credentials =
  TokenCredentials !UserId !SecretToken
  deriving (Eq, Show)

newtype SecretToken =
  SecretToken
    { secretTokenBytes :: B.ByteString
    }
  deriving (Eq, Show)

newtype SecretTokenHash =
  SecretTokenHash
    { secretTokenHashBytes :: B.ByteString
    }
  deriving (Eq, Show)

data AuthenticatedUser
  = AnonymousUser
  | IdentifiedUser !UserId !IsAdmin
  deriving (Eq, Show)

type IsAdmin = Bool
