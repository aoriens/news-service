module Web.Handler.GetUsers
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.GetUsers as I
import Core.User
import Web.Application
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle =
  Handle
    { hGetUsersHandle :: I.Handle IO
    , hPresenter :: [User] -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  users <- I.run hGetUsersHandle pageQuery
  respond $ hPresenter users
