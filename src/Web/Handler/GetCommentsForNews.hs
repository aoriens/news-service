module Web.Handler.GetCommentsForNews
  ( run
  , Handle(..)
  ) where

import Core.Comment
import qualified Core.Interactor.GetCommentsForNews as I
import Core.News
import Web.Application
import qualified Web.QueryParameter as QueryParameter
import Web.QueryParameter.PageQuery

data Handle =
  Handle
    { hGetCommentsForNewsHandle :: I.Handle IO
    , hPresent :: [Comment] -> Response
    }

run :: Handle -> NewsId -> Application
run Handle {..} newsId request respond = do
  pageSpecQuery <-
    QueryParameter.parseQueryM (requestQueryString request) parsePageQuery
  comments <- I.run hGetCommentsForNewsHandle newsId pageSpecQuery
  respond $ hPresent comments
