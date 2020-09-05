module Gateway.Authors
  ( createAuthor
  ) where

import Core.Author
import qualified Core.Interactor.CreateAuthor as I
import Core.User
import qualified Data.Text as T
import qualified Database as DB
import qualified Database.Authors as DAuthors

createAuthor :: DB.Handle -> UserId -> T.Text -> IO (Either I.Failure Author)
createAuthor h uid description =
  DB.runTransactionRW h $ DAuthors.createAuthor uid description
