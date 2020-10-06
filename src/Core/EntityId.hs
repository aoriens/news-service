module Core.EntityId
  ( EntityId(..)
  ) where

import Core.Author
import Core.Category
import Core.User

-- | A sum type to represent an identifier of an arbtirary entity.
data EntityId
  = UserEntityId UserId
  | AuthorEntityId AuthorId
  | CategoryEntityId CategoryId
  deriving (Show, Eq)
