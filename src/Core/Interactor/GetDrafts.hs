module Core.Interactor.GetDrafts
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.Deletable
import Core.Exception
import Core.News
import Core.Pagination
import Core.User

data Handle m =
  Handle
    { hGetDraftsOfAuthor :: AuthorId -> PageSpec -> m [Draft]
    , hGetDraftsOfUser :: UserId -> PageSpec -> m [Draft]
    , hAuthorizationHandle :: AuthorizationHandle
    , hPageSpecParserHandle :: PageSpecParserHandle
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> Maybe AuthorId -- ^ When Nothing, all authors related to the user will be implied
  -> PageSpecQuery
  -> m [Draft]
run Handle {..} authUser optAuthorId pageQuery = do
  userId <-
    maybe (throwM AuthenticationRequired) pure (authenticatedUserId authUser)
  pageSpec <- parsePageSpecM hPageSpecParserHandle pageQuery
  case optAuthorId of
    Nothing -> hGetDraftsOfUser userId pageSpec
    Just authorId -> do
      requirePermission
        hAuthorizationHandle
        (AuthorshipPermission $ Existing authorId)
        authUser
        "get drafts"
      hGetDraftsOfAuthor authorId pageSpec
