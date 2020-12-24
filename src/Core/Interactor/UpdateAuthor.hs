module Core.Interactor.UpdateAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.EntityId
import Core.Exception
import qualified Data.Text as T

newtype Handle m =
  Handle
    { hUpdateAuthor :: AuthorId -> T.Text -> m (Maybe Author)
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> AuthorId
  -> T.Text
  -> m Author
run Handle {..} authUser aid newDescription = do
  authorize "update author" $ authUserShouldBeAdmin authUser
  optAuthor' <- hUpdateAuthor aid newDescription
  case optAuthor' of
    Just author' -> pure author'
    Nothing -> throwM . RequestedEntityNotFoundException $ AuthorEntityId aid
