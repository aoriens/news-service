module Gateway.News
  ( getNews
  ) where

import Core.News
import Core.Pagination
import Data.Foldable
import qualified Database as DB
import qualified Database.News as DNews

getNews :: DB.Handle -> PageSpec -> IO [News]
getNews h page =
  toList <$> DB.runTransaction h (DB.statement DNews.selectNews page)
