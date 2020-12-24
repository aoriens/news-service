module Web.Handler.GetAuthors
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Author
import Core.Pagination
import Web.Application
import Web.Credentials hiding (Credentials)
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle m =
  Handle
    { hGetAuthors :: AuthenticatedUser -> PageSpecQuery -> m [Author]
    , hPresent :: [Author] -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> GenericApplication m
run Handle {..} request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  authors <- hGetAuthors authUser pageQuery
  respond $ hPresent authors
