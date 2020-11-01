module Gateway.News
  ( getNews
  , createNewsVersion
  ) where

import Core.Interactor.CreateDraft
import Core.News
import Core.Pagination
import qualified Database as DB
import qualified Database.News as DNews

getNews :: DB.Handle -> PageSpec -> IO [News]
getNews h = DB.runTransactionRO h . DNews.getNews

createNewsVersion ::
     DB.Handle
  -> CreateNewsVersionCommand
  -> IO (Either GatewayFailure NewsVersion)
createNewsVersion h = DB.runTransactionRW h . DNews.createNewsVersion
