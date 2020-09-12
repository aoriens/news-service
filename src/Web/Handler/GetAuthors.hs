module Web.Handler.GetAuthors
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.GetAuthors as I
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.Credentials
import qualified Web.HTTP as Http
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
  respond $
    Wai.responseBuilder Http.ok200 [Http.hJSONContentType] $
    presentAuthors (hPresenterHandle h) authors
