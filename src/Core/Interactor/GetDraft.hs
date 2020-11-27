module Core.Interactor.GetDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.News

data Handle m =
  Handle
    { hGetDraft :: NewsVersionId -> m (Maybe NewsVersion)
    , hAuthorizationHandle :: AuthorizationHandle
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
    requirePermission
      hAuthorizationHandle
      (AuthorshipPermission . authorId $ nvAuthor draft)
      authUser
      "get a draft"
  pure optDraft
