module Core.Authorization
  ( requiresAdminPermission
  , module Core.Authentication
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Exception
import qualified Data.Text as T

requiresAdminPermission ::
     MonadThrow m => AuthenticatedUser -> T.Text -> m a -> m a
requiresAdminPermission user actionDescription action
  | IdentifiedUser _ True <- user = action
  | otherwise =
    throwM . NoPermissionException $
    "Requires admin permission: " <> actionDescription
