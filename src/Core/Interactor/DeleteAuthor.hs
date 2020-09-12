module Core.Interactor.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization

data Handle m =
  Handle
    { hDeleteAuthor :: AuthorId -> m ()
    , hAuthHandle :: AuthenticationHandle m
    }

run :: MonadThrow m => Handle m -> Maybe Credentials -> AuthorId -> m ()
run Handle {..} credentials authorIdent = do
  actor <- authenticate hAuthHandle credentials
  requireAdminPermission actor "deleting author"
  hDeleteAuthor authorIdent
