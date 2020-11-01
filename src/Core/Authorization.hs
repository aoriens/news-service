module Core.Authorization
  ( module Core.Permission
  , AuthorizationHandle(..)
  , requireAdminPermission
  , requireAuthorshipPermission
  , module Core.Authentication
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Author
import Core.Exception
import Core.Permission
import qualified Data.Text as T

newtype AuthorizationHandle =
  AuthorizationHandle
    { hHasPermission :: Permission -> AuthenticatedUser -> Bool
    }

requireAdminPermission ::
     MonadThrow m => AuthorizationHandle -> AuthenticatedUser -> T.Text -> m ()
requireAdminPermission h = requirePermission h AdminPermission

requireAuthorshipPermission ::
     MonadThrow m
  => AuthorizationHandle
  -> AuthorId
  -> AuthenticatedUser
  -> T.Text
  -> m ()
requireAuthorshipPermission h = requirePermission h . AuthorshipPermission

requirePermission ::
     MonadThrow m
  => AuthorizationHandle
  -> Permission
  -> AuthenticatedUser
  -> T.Text
  -> m ()
requirePermission h perm user actionDescription
  | hHasPermission h perm user = pure ()
  | otherwise = throwM $ NoPermissionException perm actionDescription
