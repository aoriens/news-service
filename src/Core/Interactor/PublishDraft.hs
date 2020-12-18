module Core.Interactor.PublishDraft
  ( run
  , Handle(..)
  , Failure(..)
  , MakeDraftIntoNewsFailure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
import Core.Deletable
import Core.News
import Data.Bifunctor
import Data.Time

data Handle m =
  Handle
    { hGetDraftAuthor :: DraftId -> m (Maybe (Deletable AuthorId))
    , hGetCurrentDay :: m Day
    , hMakeDraftIntoNews :: DraftId -> Day -> m (Either MakeDraftIntoNewsFailure News)
    }

data MakeDraftIntoNewsFailure =
  MDNUnknownDraftId
  deriving (Eq, Show)

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> DraftId
  -> m (Either Failure News)
run Handle {..} authUser vId = do
  hGetDraftAuthor vId >>= \case
    Nothing -> pure $ Left UnknownDraftId
    Just authorId -> do
      authorize "publish a draft" $
        authUser `authUserShouldBeDeletableAuthor` authorId
      day <- hGetCurrentDay
      first fromMakeDraftIntoNewsFailure <$> hMakeDraftIntoNews vId day

data Failure =
  UnknownDraftId
  deriving (Eq, Show)

fromMakeDraftIntoNewsFailure :: MakeDraftIntoNewsFailure -> Failure
fromMakeDraftIntoNewsFailure MDNUnknownDraftId = UnknownDraftId
