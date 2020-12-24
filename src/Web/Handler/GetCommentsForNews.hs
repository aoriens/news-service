module Web.Handler.GetCommentsForNews
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Comment
import Core.News
import Core.Pagination
import Web.Application
import qualified Web.QueryParameter as QueryParameter
import Web.QueryParameter.PageQuery

data Handle m =
  Handle
    { hGetCommentsForNews :: NewsId -> PageSpecQuery -> m [Comment]
    , hPresent :: [Comment] -> Response
    }

run :: MonadThrow m => Handle m -> NewsId -> GenericApplication m
run Handle {..} newsId request respond = do
  pageSpecQuery <-
    QueryParameter.parseQueryM (requestQueryString request) parsePageQuery
  comments <- hGetCommentsForNews newsId pageSpecQuery
  respond $ hPresent comments
