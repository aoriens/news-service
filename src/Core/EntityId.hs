module Core.EntityId
  ( EntityId(..)
  , IsEntityId(..)
  ) where

import Core.Author
import Core.Category
import Core.Image
import Core.Tag
import Core.User

-- | A sum type to represent an identifier of an arbtirary entity.
data EntityId
  = UserEntityId UserId
  | AuthorEntityId AuthorId
  | CategoryEntityId CategoryId
  | TagEntityId TagId
  | ImageEntityId ImageId
  deriving (Show, Eq)

class IsEntityId a where
  toEntityId :: a -> EntityId

instance IsEntityId EntityId where
  toEntityId = id

instance IsEntityId UserId where
  toEntityId = UserEntityId

instance IsEntityId AuthorId where
  toEntityId = AuthorEntityId

instance IsEntityId CategoryId where
  toEntityId = CategoryEntityId

instance IsEntityId TagId where
  toEntityId = TagEntityId

instance IsEntityId ImageId where
  toEntityId = ImageEntityId
