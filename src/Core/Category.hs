module Core.Category
  ( Category(..)
  , CategoryId(..)
  ) where

import Data.Int
import Data.Text (Text)

newtype CategoryId =
  CategoryId
    { getCategoryId :: Int32
    }
  deriving (Eq, Show)

data Category =
  Category
    { categoryId :: CategoryId
    , categoryName :: Text
    , categoryParent :: Maybe Category
    }
  deriving (Eq, Show)
