module Core.Tag
  ( TagId(..)
  , Tag(..)
  ) where

import Data.Hashable
import Data.Int
import Data.Text (Text)

newtype TagId =
  TagId
    { getTagId :: Int32
    }
  deriving (Eq, Show)

instance Hashable TagId where
  hashWithSalt salt = hashWithSalt salt . getTagId

data Tag =
  Tag
    { tagId :: TagId
    , tagName :: Text
    }
  deriving (Eq, Show)

instance Hashable Tag where
  hashWithSalt salt = hashWithSalt salt . tagId
