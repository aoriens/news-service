module Core.AuthorizationNG
  ( authUserMustBeAuthor
  , authUserMustBeAdmin
  , authUserIsAuthor
  , authUserIsAdmin
  , module Core.Authentication
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Authentication
import Core.Author
import Core.Exception
import Core.Permission
import qualified Data.Text as T

authUserMustBeAuthor ::
     MonadThrow m => AuthenticatedUser -> AuthorId -> T.Text -> m ()
authUserMustBeAuthor user authorId actionDescription =
  unless (authUserIsAuthor user authorId) $
  throwM
    (NoPermissionException (AuthorshipPermission authorId) actionDescription)

authUserIsAuthor :: AuthenticatedUser -> AuthorId -> Bool
authUserIsAuthor (IdentifiedUser _ _ authorIds) requiredAuthorId =
  requiredAuthorId `elem` authorIds
authUserIsAuthor AnonymousUser _ = False

authUserMustBeAdmin :: MonadThrow m => AuthenticatedUser -> T.Text -> m ()
authUserMustBeAdmin user actionDescription =
  unless (authUserIsAdmin user) $
  throwM (NoPermissionException AdminPermission actionDescription)

authUserIsAdmin :: AuthenticatedUser -> Bool
authUserIsAdmin (IdentifiedUser _ isAdmin _) = isAdmin
authUserIsAdmin AnonymousUser = False
