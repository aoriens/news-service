module Core.Category
  ( Category(..)
  , CategoryId(..)
  , categoryAncestry
  ) where

import Control.Arrow
import Data.Hashable
import Data.Int
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

newtype CategoryId =
  CategoryId
    { getCategoryId :: Int32
    }
  deriving (Eq, Show)

instance Hashable CategoryId where
  hashWithSalt salt = hashWithSalt salt . getCategoryId

data Category =
  Category
    { categoryId :: CategoryId
    , categoryName :: Text
    , categoryParent :: Maybe Category
    }
  deriving (Eq, Show)

-- | Returns the list consisting of the category, its parent, its grandparent etc.
categoryAncestry :: Category -> NonEmpty Category
categoryAncestry = NonEmpty.unfoldr (id &&& categoryParent)
