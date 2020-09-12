module Core.EntityId
  ( EntityId(..)
  ) where

import Core.Author
import Core.User

-- | A sum type to represent an identifier of an arbtirary entity.
data EntityId
  = UserEntityId UserId
  | AuthorEntityId AuthorId
  deriving (Show, Eq)
