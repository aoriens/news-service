module Database.Authors where

import Core.Author
import Core.Pagination
import Core.User
import Data.Vector (Vector)
import Database

selectAuthorsByUserId :: Statement (UserId, PageSpec) (Vector AuthorId)
