module Web.Handler.GetDrafts
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Author
import Core.News
import Core.Pagination
import Web.Application
import Web.Credentials hiding (Credentials)
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle m =
  Handle
    { hGetDrafts :: AuthenticatedUser -> Maybe AuthorId -> PageSpecQuery -> m [Draft]
    -- ^ When Maybe AuthorId is Nothing, all authors related to the
    -- user are implied
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    , hPresent :: [Draft] -> Response
    }

-- | When 'Nothing' passed as 'Maybe AuthorId', all authors of the
-- authenticated user will be considered.
run :: MonadThrow m => Handle m -> Maybe AuthorId -> GenericApplication m
run Handle {..} optAuthorId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  drafts <- hGetDrafts authUser optAuthorId pageQuery
  respond $ hPresent drafts
