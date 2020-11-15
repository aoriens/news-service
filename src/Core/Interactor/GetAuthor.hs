module Core.Interactor.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization

data Handle m =
  Handle
    { hGetAuthor :: AuthorId -> m (Maybe Author)
    , hAuthorizationHandle :: AuthorizationHandle
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> AuthorId
  -> m (Maybe Author)
run Handle {..} authUser authorId' = do
  requireAdminPermission hAuthorizationHandle authUser "get an author"
  hGetAuthor authorId'
