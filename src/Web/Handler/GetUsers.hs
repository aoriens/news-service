module Web.Handler.GetUsers
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Pagination
import Core.User
import Web.Application
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle m =
  Handle
    { hGetUsers :: PageSpecQuery -> m [User]
    , hPresent :: [User] -> Response
    }

run :: MonadThrow m => Handle m -> GenericApplication m
run Handle {..} request respond = do
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  users <- hGetUsers pageQuery
  respond $ hPresent users
