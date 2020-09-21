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
    , hAuthHandle :: AuthenticationHandle m
    , hAuthorizationHandle :: AuthorizationHandle
    }

type Success = Bool

run :: MonadThrow m => Handle m -> Maybe Credentials -> AuthorId -> m ()
run Handle {..} credentials authorIdent = do
  actor <- authenticate hAuthHandle credentials
  requireAdminPermission hAuthorizationHandle actor "deleting author"
  ok <- hDeleteAuthor authorIdent
  unless ok $ throwM . EntityNotFoundException $ AuthorEntityId authorIdent
