module Core.Tag
  ( TagId(..)
  , Tag(..)
  ) where

import Data.Int
import Data.Text (Text)

newtype TagId =
  TagId
    { getTagId :: Int32
    }
  deriving (Eq, Show)

data Tag =
  Tag
    { tagId :: TagId
    , tagName :: Text
    }
  deriving (Eq, Show)
