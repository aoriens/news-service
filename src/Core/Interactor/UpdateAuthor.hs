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

data Handle m =
  Handle
    { hAuthHandle :: AuthenticationHandle m
    , hUpdateAuthor :: AuthorId -> T.Text -> m (Maybe Author)
    }

run ::
     MonadThrow m
  => Handle m
  -> Maybe Credentials
  -> AuthorId
  -> T.Text
  -> m Author
run Handle {..} credentials aid newDescription = do
  actor <- authenticate hAuthHandle credentials
  requireAdminPermission actor "update author"
  optAuthor' <- hUpdateAuthor aid newDescription
  case optAuthor' of
    Just author' -> pure author'
    Nothing -> throwM . EntityNotFoundException $ AuthorEntityId aid
