module Web.Handler.GetAuthors
  ( run
  , Handle(..)
  ) where

import Core.Authentication
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
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> Application
run Handle {..} request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  authors <- I.run hGetAuthorsHandle authUser pageQuery
  respond $ hPresenter authors
