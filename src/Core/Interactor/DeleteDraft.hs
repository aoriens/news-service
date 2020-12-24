module Core.Interactor.DeleteDraft
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.Deletable
import Core.News

data Handle m =
  Handle
    { hGetDraftAuthor :: DraftId -> m (Maybe (Deletable AuthorId))
    , hDeleteDraftAndItsContent :: DraftId -> m ()
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> DraftId
  -> m (Either Failure ())
run Handle {..} authUser draftId = do
  hGetDraftAuthor draftId >>= \case
    Nothing -> pure $ Left UnknownDraftId
    Just authorId -> do
      authorize "deleting a draft" $
        authUser `authUserShouldBeDeletableAuthor` authorId
      hDeleteDraftAndItsContent draftId
      pure $ Right ()

data Failure =
  UnknownDraftId
  deriving (Eq, Show)
