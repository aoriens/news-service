{-# LANGUAGE RankNTypes #-}

module Core.Authentication
  ( AuthenticationHandle(..)
  , Credentials(..)
  , SecretToken(..)
  , SecretTokenHash(..)
  , AuthenticatedUser(..)
  , IsAdmin
  , authenticatedUserId
  , authenticatedUserIsAdmin
  ) where

import Control.Monad.Catch
import Core.Author
import Core.User
import qualified Data.ByteString as B

newtype AuthenticationHandle m =
  AuthenticationHandle
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
  | IdentifiedUser !UserId !IsAdmin [AuthorId]
  deriving (Eq, Show)

type IsAdmin = Bool

authenticatedUserId :: AuthenticatedUser -> Maybe UserId
authenticatedUserId AnonymousUser = Nothing
authenticatedUserId (IdentifiedUser uid _ _) = Just uid

authenticatedUserIsAdmin :: AuthenticatedUser -> Bool
authenticatedUserIsAdmin AnonymousUser = False
authenticatedUserIsAdmin (IdentifiedUser _ isAdmin _) = isAdmin
