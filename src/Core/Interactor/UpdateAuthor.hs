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
    { hAuthenticationHandle :: AuthenticationHandle m
    , hAuthorizationHandle :: AuthorizationHandle
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
  actor <- authenticate hAuthenticationHandle credentials
  requireAdminPermission hAuthorizationHandle actor "update author"
  optAuthor' <- hUpdateAuthor aid newDescription
  case optAuthor' of
    Just author' -> pure author'
    Nothing -> throwM . EntityNotFoundException $ AuthorEntityId aid
