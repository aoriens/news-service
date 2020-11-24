module Core.Interactor.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.EntityId
import Core.Exception

data Handle m =
  Handle
    { hDeleteAuthor :: AuthorId -> m Success
    , hAuthorizationHandle :: AuthorizationHandle
    }

type Success = Bool

run :: MonadThrow m => Handle m -> AuthenticatedUser -> AuthorId -> m ()
run Handle {..} authUser authorId' = do
  requirePermission
    hAuthorizationHandle
    AdminPermission
    authUser
    "deleting author"
  ok <- hDeleteAuthor authorId'
  unless ok $
    throwM . RequestedEntityNotFoundException $ AuthorEntityId authorId'
