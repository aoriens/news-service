module Core.Interactor.PublishDraft
  ( run
  , Handle(..)
  , Failure(..)
  , MakeDraftIntoNewsFailure(..)
  , OverwriteNewsWithDraftFailure(..)
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
    { hGetDraftAuthorAndNewsIdItWasCreatedFrom :: DraftId -> m (Maybe ( Deletable AuthorId
                                                                      , Maybe NewsId))
    , hGetCurrentDay :: m Day
    , hMakeDraftIntoNews :: DraftId -> Day -> m (Either MakeDraftIntoNewsFailure News)
    , hOverwriteNewsWithDraft :: NewsId -> DraftId -> Day -> m (Either OverwriteNewsWithDraftFailure News)
    }

data MakeDraftIntoNewsFailure =
  MDNUnknownDraftId
  deriving (Eq, Show)

data OverwriteNewsWithDraftFailure =
  ONDUnknownDraftId
  deriving (Eq, Show)

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> DraftId
  -> m (Either Failure News)
run Handle {..} authUser draftId = do
  hGetDraftAuthorAndNewsIdItWasCreatedFrom draftId >>= \case
    Nothing -> pure $ Left UnknownDraftId
    Just (authorId, originNewsId) -> do
      authorize "publish a draft" $
        authUser `authUserShouldBeDeletableAuthor` authorId
      day <- hGetCurrentDay
      case originNewsId of
        Nothing ->
          first fromMakeDraftIntoNewsFailure <$> hMakeDraftIntoNews draftId day
        Just newsId ->
          first fromOverwriteNewsWithDraftFailure <$>
          hOverwriteNewsWithDraft newsId draftId day

data Failure =
  UnknownDraftId
  deriving (Eq, Show)

fromMakeDraftIntoNewsFailure :: MakeDraftIntoNewsFailure -> Failure
fromMakeDraftIntoNewsFailure MDNUnknownDraftId = UnknownDraftId

fromOverwriteNewsWithDraftFailure :: OverwriteNewsWithDraftFailure -> Failure
fromOverwriteNewsWithDraftFailure ONDUnknownDraftId = UnknownDraftId
