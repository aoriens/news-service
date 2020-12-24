module Core.Interactor.DeleteAuthor
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization

data Handle m =
  Handle
    { hDeleteAuthor :: AuthorId -> m (Either Failure ())
    , hDeleteDraftsOfAuthor :: AuthorId -> m ()
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> AuthorId
  -> m (Either Failure ())
run Handle {..} authUser authorId = do
  authorize "delete an author" $ authUserShouldBeAdmin authUser
  hDeleteDraftsOfAuthor authorId
  hDeleteAuthor authorId

data Failure =
  UnknownAuthorId
  deriving (Eq, Show)
