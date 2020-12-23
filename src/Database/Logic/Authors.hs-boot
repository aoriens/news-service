module Database.Logic.Authors where

import Core.Author
import Core.Pagination
import Core.User
import Database.Service.Primitives

getAuthorsByUserId :: UserId -> Maybe PageSpec -> Transaction [AuthorId]
