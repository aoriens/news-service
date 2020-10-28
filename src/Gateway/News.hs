module Gateway.News
  ( getNews
  ) where

import Core.News
import Core.Pagination
import qualified Database as DB
import qualified Database.News as DNews

getNews :: DB.Handle -> PageSpec -> IO [News]
getNews h = DB.runTransactionRO h . DNews.getNews
