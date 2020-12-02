module Core.Author
  ( Author(..)
  , AuthorId(..)
  ) where

import Core.Deletable
import Core.User
import Data.Hashable
import Data.Int
import qualified Data.Text as T

newtype AuthorId =
  AuthorId
    { getAuthorId :: Int32
    }
  deriving (Eq, Show)

instance Hashable AuthorId where
  hashWithSalt salt = hashWithSalt salt . getAuthorId

data Author =
  Author
    { authorId :: AuthorId
    , authorUser :: Deletable User
    , authorDescription :: T.Text
    }
  deriving (Eq, Show)
