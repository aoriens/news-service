module Web.Handler.GetCategories
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Category
import Core.Pagination
import Web.Application
import Web.QueryParameter
import Web.QueryParameter.PageQuery

data Handle m =
  Handle
    { hGetCategories :: PageSpecQuery -> m [Category]
    , hPresent :: [Category] -> Response
    }

run :: MonadThrow m => Handle m -> GenericApplication m
run Handle {..} request respond = do
  pageSpecQuery <- parseQueryM (requestQueryString request) parsePageQuery
  categories <- hGetCategories pageSpecQuery
  respond $ hPresent categories
