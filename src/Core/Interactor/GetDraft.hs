module Core.Interactor.GetDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Core.Author
import Core.Authorization
import Core.Deletable
import Core.News

newtype Handle m =
  Handle
    { hGetDraft :: DraftId -> m (Maybe Draft)
    }

run ::
     MonadThrow m => Handle m -> AuthenticatedUser -> DraftId -> m (Maybe Draft)
run Handle {..} authUser draftId =
  runMaybeT $ do
    draft <- MaybeT $ hGetDraft draftId
    author <- MaybeT . pure . maybeFromDeletable . nvAuthor $ draftContent draft
    authorize "get a draft" $ authUserShouldBeAuthor authUser (authorId author)
    pure draft
