module Core.Interactor.DeleteDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
import Core.EntityId
import Core.Exception
import Core.News

data Handle m =
  Handle
    { hGetDraftAuthor :: NewsVersionId -> m (Maybe AuthorId)
    , hDeleteNewsVersion :: NewsVersionId -> m ()
    }

run :: MonadThrow m => Handle m -> AuthenticatedUser -> NewsVersionId -> m ()
run Handle {..} authUser draftId = do
  authorId <-
    maybe (throwM $ RequestedEntityNotFoundException $ toEntityId draftId) pure =<<
    hGetDraftAuthor draftId
  authUserMustBeAuthor authUser authorId "deleting a draft"
  hDeleteNewsVersion draftId
