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
    , hAuthHandle :: AuthenticationHandle m
    }

run ::
     MonadThrow m
  => Handle m
  -> Maybe Credentials
  -> AuthorId
  -> m (Maybe Author)
run Handle {..} credentials authorIdent = do
  actor <- authenticate hAuthHandle credentials
  requireAdminPermission hAuthorizationHandle actor "get an author"
  hGetAuthor authorIdent
