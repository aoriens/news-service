module Core.Authorization
  ( requireAdminPermission
  , hasAdminPermission
  , module Core.Authentication
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Exception
import qualified Data.Text as T

requireAdminPermission :: MonadThrow m => AuthenticatedUser -> T.Text -> m ()
requireAdminPermission user actionDescription
  | hasAdminPermission user = pure ()
  | otherwise =
    throwM . NoPermissionException $
    "Requires admin permission: " <> actionDescription

hasAdminPermission :: AuthenticatedUser -> Bool
hasAdminPermission (IdentifiedUser _ True) = True
hasAdminPermission (IdentifiedUser _ False) = False
hasAdminPermission AnonymousUser = False
