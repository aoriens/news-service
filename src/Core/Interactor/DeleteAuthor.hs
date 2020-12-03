module Core.Interactor.DeleteAuthor
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG

newtype Handle m =
  Handle
    { hDeleteAuthor :: AuthorId -> m (Either Failure ())
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> AuthorId
  -> m (Either Failure ())
run Handle {..} authUser authorId = do
  authorize "delete an author" $ authUserShouldBeAdmin authUser
  hDeleteAuthor authorId

data Failure =
  UnknownAuthorId
  deriving (Eq, Show)
