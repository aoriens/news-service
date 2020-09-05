module Core.Author
  ( Author(..)
  , AuthorId(..)
  ) where

import Core.User
import Data.Int
import qualified Data.Text as T

newtype AuthorId =
  AuthorId
    { getAuthorId :: Int32
    }
  deriving (Eq, Show)

data Author =
  Author
    { authorId :: AuthorId
    , authorUser :: User
    , authorDescription :: T.Text
    }
  deriving (Eq, Show)
