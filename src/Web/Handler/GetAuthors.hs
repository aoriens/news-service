module Web.Handler.GetAuthors
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.GetAuthors as I
import qualified Network.Wai as Wai
import Web.Credentials
import Web.Presenter.Author
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle =
  Handle
    { hGetAuthorsHandle :: I.Handle IO
    , hPresenterHandle :: RepBuilderHandle
    }

run :: Handle -> Wai.Application
run h request respond = do
  credentials <- getCredentialsFromRequest request
  pageQuery <- QP.parseQueryM (Wai.queryString request) QP.parsePageQuery
  authors <- I.run (hGetAuthorsHandle h) credentials pageQuery
  respond $ presentAuthors (hPresenterHandle h) authors
