module Core.Interactor.PublishDraft
  ( run
  , Handle(..)
  , Failure(..)
  , Success(..)
  , Status(..)
  , MakeDraftIntoNewsFailure(..)
  , OverwriteNewsWithDraftFailure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
import Core.Deletable
import Core.News
import Data.Functor
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
  -> m (Either Failure Success)
run h@Handle {..} authUser draftId = do
  hGetDraftAuthorAndNewsIdItWasCreatedFrom draftId >>= \case
    Nothing -> pure $ Left UnknownDraftId
    Just (authorId, originNewsId) -> do
      authorize "publish a draft" $
        authUser `authUserShouldBeDeletableAuthor` authorId
      day <- hGetCurrentDay
      case originNewsId of
        Nothing -> createNews h draftId day
        Just newsId -> updateNews h newsId draftId day

data Success =
  Success
    { sNews :: News
    , sStatus :: Status
    }
  deriving (Eq, Show)

data Status
  = NewsIsCreated
  | NewsIsUpdated
  deriving (Eq, Show)

data Failure =
  UnknownDraftId
  deriving (Eq, Show)

createNews ::
     Functor m => Handle m -> DraftId -> Day -> m (Either Failure Success)
createNews Handle {..} draftId day =
  hMakeDraftIntoNews draftId day <&> \case
    Left MDNUnknownDraftId -> Left UnknownDraftId
    Right news -> Right Success {sNews = news, sStatus = NewsIsCreated}

updateNews ::
     Functor m
  => Handle m
  -> NewsId
  -> DraftId
  -> Day
  -> m (Either Failure Success)
updateNews Handle {..} newsId draftId day =
  hOverwriteNewsWithDraft newsId draftId day <&> \case
    Left ONDUnknownDraftId -> Left UnknownDraftId
    Right news -> Right Success {sNews = news, sStatus = NewsIsUpdated}
