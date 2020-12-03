module Core.Interactor.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG

newtype Handle m =
  Handle
    { hDeleteAuthor :: AuthorId -> m Success
    }

type Success = Bool

-- | Returns 'False' when no such author is found.
run :: MonadThrow m => Handle m -> AuthenticatedUser -> AuthorId -> m Success
run Handle {..} authUser authorId = do
  authorize "delete an author" $ authUserShouldBeAdmin authUser
  hDeleteAuthor authorId
