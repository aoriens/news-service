module Core.Interactor.GetDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
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
run Handle {..} authUser draftId = do
  optDraft <- hGetDraft draftId
  forM_ optDraft $ \draft ->
    authorize "get a draft" $
    authUserShouldBeAuthor authUser (authorId $ nvAuthor draft)
  pure optDraft
