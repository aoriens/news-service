module Core.Interactor.PublishDraft
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
import Core.Deletable
import Core.News
import Data.Time

data Handle m =
  Handle
    { hGetDraftAuthor :: NewsVersionId -> m (Maybe (Deletable AuthorId))
    , hGetCurrentDay :: m Day
    , hCreateNews :: NewsVersionId -> Day -> m News
    }

-- | Returns Nothing when no draft is found.
run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> NewsVersionId
  -> m (Either Failure News)
run Handle {..} authUser vId = do
  hGetDraftAuthor vId >>= \case
    Nothing -> pure $ Left UnknownDraftId
    Just Deleted -> pure $ Left UnknownDraftId
    Just (Existing authorId) -> do
      authorize "publish a draft" $ authUserShouldBeAuthor authUser authorId
      day <- hGetCurrentDay
      Right <$> hCreateNews vId day

data Failure =
  UnknownDraftId
  deriving (Eq, Show)
