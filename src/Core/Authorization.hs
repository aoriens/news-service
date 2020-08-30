module Core.Authorization
  ( requiresAdminPermission
  , NoPermissionException
  , module Core.Authentication
  ) where

import Control.Monad.Catch
import Core.Authentication

requiresAdminPermission :: MonadThrow m => AuthenticatedUser -> m () -> m ()
requiresAdminPermission (IdentifiedUser _ (IsAdmin True)) action = action
requiresAdminPermission _ _ = throwM NoPermissionException

data NoPermissionException =
  NoPermissionException
  deriving (Show)

instance Exception NoPermissionException
