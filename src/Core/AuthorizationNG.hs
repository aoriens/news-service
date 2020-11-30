module Core.AuthorizationNG
  ( authorize
  , authUserShouldBeAdmin
  , authUserShouldBeAuthor
  , module Core.Authentication
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Authentication
import Core.Author
import Core.Exception
import Core.Permission
import qualified Data.Text as T

type ActionDescription = T.Text

-- | The result of a permission check and the permission description.
type PermissionCheck = (Bool, Permission)

-- | Checks a condition and throws authorization exception when the
-- condition is 'False'.
authorize :: MonadThrow m => ActionDescription -> PermissionCheck -> m ()
authorize actionDescription (isAuthorized, perm) =
  unless isAuthorized $ throwM (NoPermissionException perm actionDescription)

authUserShouldBeAuthor :: AuthenticatedUser -> AuthorId -> PermissionCheck
authUserShouldBeAuthor user requiredAuthorId =
  (r, AuthorshipPermission requiredAuthorId)
  where
    r =
      case user of
        IdentifiedUser _ _ authorIds -> requiredAuthorId `elem` authorIds
        AnonymousUser -> False

authUserShouldBeAdmin :: AuthenticatedUser -> PermissionCheck
authUserShouldBeAdmin user = (r, AdminPermission)
  where
    r =
      case user of
        IdentifiedUser _ isAdmin _ -> isAdmin
        AnonymousUser -> False
