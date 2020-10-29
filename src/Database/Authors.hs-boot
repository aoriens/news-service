module Database.Authors where

import Core.Author
import Core.Pagination
import Core.User
import Database

selectAuthorsByUserId :: UserId -> PageSpec -> Transaction [AuthorId]
