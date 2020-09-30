module Web.Handler.GetUsers
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.GetUsers as I
import Core.User
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP
import Web.Types

data Handle =
  Handle
    { hGetUsersHandle :: I.Handle IO
    , hPresenter :: [User] -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  pageQuery <- QP.parseQueryM (queryString request) QP.parsePageQuery
  users <- I.run hGetUsersHandle pageQuery
  respond $ hPresenter users
