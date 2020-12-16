module Core.AuthorizationNG
  ( authorize
  , authUserShouldBeAdmin
  , authUserShouldBeAuthor
  , authUserShouldBeDeletableAuthor
  , authUserShouldBeAdminOrSpecificUser
  , module Core.Authentication
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Authentication
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Permission
import Core.User
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
authUserShouldBeAuthor user = authUserShouldBeDeletableAuthor user . Existing

authUserShouldBeDeletableAuthor ::
     AuthenticatedUser -> Deletable AuthorId -> PermissionCheck
authUserShouldBeDeletableAuthor user requiredAuthorId =
  (r, AuthorshipPermission requiredAuthorId)
  where
    r =
      case (requiredAuthorId, user) of
        (Existing authorId, IdentifiedUser _ _ authorIds) ->
          authorId `elem` authorIds
        (Deleted, _) -> False
        (_, AnonymousUser) -> False

authUserShouldBeAdmin :: AuthenticatedUser -> PermissionCheck
authUserShouldBeAdmin user = (r, AdminPermission)
  where
    r =
      case user of
        IdentifiedUser _ isAdmin _ -> isAdmin
        AnonymousUser -> False

authUserShouldBeAdminOrSpecificUser ::
     AuthenticatedUser -> UserId -> PermissionCheck
authUserShouldBeAdminOrSpecificUser authUser requiredUserId =
  (r, AdminOrSpecificUserPermission requiredUserId)
  where
    r =
      case authUser of
        IdentifiedUser userId isAdmin _ -> isAdmin || userId == requiredUserId
        AnonymousUser -> False
