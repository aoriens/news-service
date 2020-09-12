module Web.Handler.GetAuthors
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.GetAuthors as I
import qualified Network.Wai as Wai
import Web.Credentials
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP
import Web.Representation.Author
import Web.RepresentationBuilder

data Handle =
  Handle
    { hGetAuthorsHandle :: I.Handle IO
    , hPresenterHandle :: RepBuilderHandle
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  credentials <- getCredentialsFromRequest request
  pageQuery <- QP.parseQueryM (Wai.queryString request) QP.parsePageQuery
  authors <- I.run hGetAuthorsHandle credentials pageQuery
  respond $ runRepBuilder hPresenterHandle $ mapM authorRepresentation authors
