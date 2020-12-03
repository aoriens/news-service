module Core.Interactor.DeleteDraft
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
import Core.News

data Handle m =
  Handle
    { hGetDraftAuthor :: NewsVersionId -> m (Maybe AuthorId)
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
    Just authorId -> do
      authorize "deleting a draft" $ authUserShouldBeAuthor authUser authorId
      hDeleteNewsVersion draftId
      pure $ Right ()

data Failure =
  UnknownDraftId
  deriving (Eq, Show)
