module Web.Handler.GetDrafts
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import Core.Author
import qualified Core.Interactor.GetDrafts as I
import Core.News
import Web.Application
import Web.Credentials
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle =
  Handle
    { hGetDraftsHandle :: I.Handle IO
    , hAuthenticationHandle :: AuthenticationHandle IO
    , hPresent :: [Draft] -> Response
    }

-- | When 'Nothing' passed as 'Maybe AuthorId', all authors of the
-- authenticated user will be considered.
run :: Handle -> Maybe AuthorId -> Application
run Handle {..} optAuthorId request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  drafts <- I.run hGetDraftsHandle authUser optAuthorId pageQuery
  respond $ hPresent drafts
