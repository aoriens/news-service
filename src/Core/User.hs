module Core.User
  ( User(..)
  , UserId(..)
  ) where

import Core.Image
import Data.Int
import Data.Text (Text)
import Data.Time

data User =
  User
    { userId :: UserId
      -- ^ The first name. This is unnecessary in case of a single-component name.
    , userFirstName :: Maybe Text
    , userLastName :: Text
    , userAvatarId :: Maybe ImageId
    , userCreatedAt :: UTCTime
    , userIsAdmin :: Bool
    }

newtype UserId =
  UserId
    { getUserId :: Int32
    }
  deriving (Eq, Show)
