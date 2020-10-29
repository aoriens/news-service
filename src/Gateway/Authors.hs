module Gateway.Authors
  ( createAuthor
  , getAuthors
  , getAuthor
  , deleteAuthor
  , updateAuthor
  ) where

import Core.Author
import qualified Core.Interactor.CreateAuthor as I
import Core.Pagination
import Core.User
import Data.Foldable
import qualified Data.Text as T
import Database as DB
import qualified Database.Authors as DAuthors

createAuthor :: DB.Handle -> UserId -> T.Text -> IO (Either I.Failure Author)
createAuthor h uid description =
  runTransactionRW h $ DAuthors.createAuthor uid description

getAuthors :: DB.Handle -> PageSpec -> IO [Author]
getAuthors h page = toList <$> runTransactionRO h (DAuthors.selectAuthors page)

getAuthor :: DB.Handle -> AuthorId -> IO (Maybe Author)
getAuthor h authorIdent =
  runTransactionRO h (DAuthors.selectAuthorById authorIdent)

deleteAuthor :: DB.Handle -> AuthorId -> IO Bool
deleteAuthor h = runTransactionRW h . fmap (0 /=) . DAuthors.deleteAuthorById

updateAuthor :: DB.Handle -> AuthorId -> T.Text -> IO (Maybe Author)
updateAuthor h aid newDescription =
  runTransactionRW h $ do
    DAuthors.updateAuthor aid newDescription
    DAuthors.selectAuthorById aid
