module Core.Interactor.DeleteDraft
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
import Core.Deletable
import Core.News

data Handle m =
  Handle
    { hGetDraftAuthor :: NewsVersionId -> m (Maybe (Deletable AuthorId))
    , hDeleteNewsVersion :: NewsVersionId -> m ()
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> NewsVersionId
  -> m (Either Failure ())
run Handle {..} authUser draftId = do
  hGetDraftAuthor draftId >>= \case
    Nothing -> pure $ Left UnknownDraftId
     -- There must not be authorless drafts, they should be deleted as
     -- soon as their author is. So, reporting it as an unknown draft.
    Just Deleted -> pure $ Left UnknownDraftId
    Just (Existing authorId) -> do
      authorize "deleting a draft" $ authUserShouldBeAuthor authUser authorId
      hDeleteNewsVersion draftId
      pure $ Right ()

data Failure =
  UnknownDraftId
  deriving (Eq, Show)
