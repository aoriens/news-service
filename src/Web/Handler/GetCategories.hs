module Web.Handler.GetCategories
  ( run
  , Handle(..)
  ) where

import Core.Category
import qualified Core.Interactor.GetCategories as IGetCategories
import Web.Application
import Web.QueryParameter
import Web.QueryParameter.PageQuery

data Handle =
  Handle
    { hGetCategoriesHandle :: IGetCategories.Handle IO
    , hPresenter :: [Category] -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  pageSpecQuery <- parseQueryM (requestQueryString request) parsePageQuery
  categories <- IGetCategories.run hGetCategoriesHandle pageSpecQuery
  respond $ hPresenter categories
