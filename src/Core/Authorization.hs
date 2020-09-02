module Core.Authorization
  ( requiresAdminPermission
  , NoPermissionException
  , module Core.Authentication
  ) where

import Control.Monad.Catch
import Core.Authentication
import qualified Data.Text as T

requiresAdminPermission ::
     MonadThrow m => AuthenticatedUser -> T.Text -> m () -> m ()
requiresAdminPermission user actionDescription action
  | IdentifiedUser _ (IsAdmin True) <- user = action
  | otherwise =
    throwM . NoPermissionException $
    "Requires admin permission: " <> actionDescription

newtype NoPermissionException =
  NoPermissionException
    { noPermissionExceptionReason :: T.Text
    }
  deriving (Show)

instance Exception NoPermissionException
