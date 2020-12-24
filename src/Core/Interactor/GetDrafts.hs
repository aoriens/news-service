module Core.Interactor.GetDrafts
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.AuthorizationNG
import Core.Exception
import Core.News
import Core.Pagination
import Core.User
import Data.Maybe.Util

data Handle m =
  Handle
    { hGetDraftsOfAuthor :: AuthorId -> PageSpec -> m [Draft]
    , hGetDraftsOfUser :: UserId -> PageSpec -> m [Draft]
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
    fromMaybeM (throwM AuthenticationRequiredException) $
    authenticatedUserId authUser
  pageSpec <- parsePageSpecM hPageSpecParserHandle pageQuery
  case optAuthorId of
    Nothing -> hGetDraftsOfUser userId pageSpec
    Just authorId -> do
      authorize "get drafts" $ authUser `authUserShouldBeAuthor` authorId
      hGetDraftsOfAuthor authorId pageSpec
