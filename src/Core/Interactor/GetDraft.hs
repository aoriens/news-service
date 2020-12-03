module Core.Interactor.GetDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Core.Author
import Core.AuthorizationNG
import Core.Deletable
import Core.News

newtype Handle m =
  Handle
    { hGetDraft :: NewsVersionId -> m (Maybe NewsVersion)
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> NewsVersionId
  -> m (Maybe NewsVersion)
run Handle {..} authUser draftId =
  runMaybeT $ do
    draft <- MaybeT $ hGetDraft draftId
    author <- MaybeT . pure . maybeFromDeletable $ nvAuthor draft
    authorize "get a draft" $ authUserShouldBeAuthor authUser (authorId author)
    pure draft
