module Web.Handler.GetAuthors
  ( run
  , Handle(..)
  ) where

import Core.Author
import qualified Core.Interactor.GetAuthors as I
import Web.Application
import Web.Credentials
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle =
  Handle
    { hGetAuthorsHandle :: I.Handle IO
    , hPresenter :: [Author] -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  credentials <- getCredentialsFromRequest request
  pageQuery <- QP.parseQueryM (queryString request) QP.parsePageQuery
  authors <- I.run hGetAuthorsHandle credentials pageQuery
  respond $ hPresenter authors
