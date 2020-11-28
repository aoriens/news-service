module Core.Interactor.PublishDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.EntityId
import Core.Exception
import Core.News
import Data.Time

data Handle m =
  Handle
    { hAuthorizationHandle :: AuthorizationHandle
    , hGetDraftAuthor :: NewsVersionId -> m (Maybe AuthorId)
    , hGetCurrentDay :: m Day
    , hCreateNews :: NewsVersionId -> Day -> m News
    }

run :: MonadThrow m => Handle m -> AuthenticatedUser -> NewsVersionId -> m News
run Handle {..} authUser vId = do
  documentAuthorId <-
    maybe (throwM . RequestedEntityNotFoundException $ toEntityId vId) pure =<<
    hGetDraftAuthor vId
  requirePermission
    hAuthorizationHandle
    (AuthorshipPermission documentAuthorId)
    authUser
    "publish a draft"
  day <- hGetCurrentDay
  hCreateNews vId day
