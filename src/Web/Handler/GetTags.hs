module Web.Handler.GetTags
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Pagination
import Core.Tag
import Web.Application
import qualified Web.QueryParameter as QP
import qualified Web.QueryParameter.PageQuery as QP

data Handle m =
  Handle
    { hGetTags :: PageSpecQuery -> m [Tag]
    , hPresent :: [Tag] -> Response
    }

run :: MonadThrow m => Handle m -> GenericApplication m
run Handle {..} request respond = do
  pageQuery <- QP.parseQueryM (requestQueryString request) QP.parsePageQuery
  tags <- hGetTags pageQuery
  respond $ hPresent tags
