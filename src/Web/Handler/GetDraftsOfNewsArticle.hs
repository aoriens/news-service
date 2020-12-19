module Web.Handler.GetDraftsOfNewsArticle
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.News
import Core.Pagination
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception
import qualified Web.QueryParameter
import Web.QueryParameter.PageQuery

data Handle m =
  Handle
    { hGetDraftsOfNewsArticle :: AuthenticatedUser -> NewsId -> PageSpec -> m (Maybe [Draft])
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    , hParsePageSpec :: PageSpecQuery -> m PageSpec
    , hPresent :: [Draft] -> Response
    }

-- | When 'Nothing' passed as 'Maybe AuthorId', all authors of the
-- authenticated user will be considered.
run :: MonadThrow m => Handle m -> NewsId -> GenericApplication m
run Handle {..} newsId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  pageSpec <-
    hParsePageSpec =<<
    Web.QueryParameter.parseQueryM (requestQueryString request) parsePageQuery
  hGetDraftsOfNewsArticle authUser newsId pageSpec >>= \case
    Nothing -> throwM NotFoundException
    Just drafts -> respond $ hPresent drafts
