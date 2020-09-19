module Web.Handler.GetUsers
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.GetUsers as I
import Core.User
import qualified Network.Wai as Wai
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle =
  Handle
    { hGetUsersHandle :: I.Handle IO
    , hPresenter :: [User] -> Wai.Response
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  pageQuery <- QP.parseQueryM (Wai.queryString request) QP.parsePageQuery
  users <- I.run hGetUsersHandle pageQuery
  respond $ hPresenter users
