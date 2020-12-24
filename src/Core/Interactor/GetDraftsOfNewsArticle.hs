module Core.Interactor.GetDraftsOfNewsArticle
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
import Core.Deletable
import Core.News
import Core.Pagination

data Handle m =
  Handle
    { hGetNewsAuthorId :: NewsId -> m (Maybe (Deletable AuthorId))
    , hGetDraftsCreatedFromNewsId :: NewsId -> PageSpec -> m [Draft]
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> NewsId
  -> PageSpec
  -> m (Maybe [Draft])
run Handle {..} authUser newsId pageSpec =
  hGetNewsAuthorId newsId >>= \case
    Nothing -> pure Nothing
    Just authorId@Deleted -> do
      authorize "get drafts for a news" $
        authUser `authUserShouldBeDeletableAuthor` authorId
      -- The next line must not ever be reached, since a Deleted
      -- author must not pass the authorization. Otherwise the empty
      -- list is a good result.
      pure $ Just []
    Just (Existing authorId) -> do
      authorize "get drafts for a news" $
        authUser `authUserShouldBeAuthor` authorId
      Just <$> hGetDraftsCreatedFromNewsId newsId pageSpec
