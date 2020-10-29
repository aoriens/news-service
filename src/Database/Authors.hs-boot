module Database.Authors where

import Core.Author
import Core.Pagination
import Core.User
import Data.Vector (Vector)
import Database

selectAuthorsByUserId :: UserId -> PageSpec -> Transaction (Vector AuthorId)
