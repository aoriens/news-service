module Core.Interactor.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization

newtype Handle m =
  Handle
    { hGetAuthor :: AuthorId -> m (Maybe Author)
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> AuthorId
  -> m (Maybe Author)
run Handle {..} authUser authorId' = do
  authorize "get an author" $ authUserShouldBeAdmin authUser
  hGetAuthor authorId'
