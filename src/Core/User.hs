{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.User
  ( User(..)
  , UserId(..)
  ) where

import Core.Image
import Data.Hashable
import Data.Int
import Data.Text (Text)
import Data.Time

data User =
  User
    { userId :: UserId
    , userFirstName :: Maybe Text
      -- ^ The first name. This is unnecessary in case of a single-component name.
    , userLastName :: Text
    , userAvatarId :: Maybe ImageId
    , userCreatedAt :: UTCTime
    , userIsAdmin :: Bool
    }
  deriving (Eq, Show)

newtype UserId =
  UserId
    { getUserId :: Int32
    }
  deriving (Eq, Show, Hashable)
