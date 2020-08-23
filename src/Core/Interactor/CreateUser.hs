{-# LANGUAGE RecordWildCards #-}

module Core.Interactor.CreateUser
  ( run
  , Handle(..)
  -- * Interactor input
  , Query(..)
  , ImageQuery
  -- * Interactor output
  , Image(..)
  , User(..)
  , UserId(..)
  , ImageId(..)
  , SecretTokenInfo(..)
  , SecretToken(..)
  -- * Gateway API
  , CreateUserCommand(..)
  , CreateUserResult(..)
  ) where

import Data.ByteString as BS
import Data.Int
import Data.Text (Text)
import Data.Time.Clock

data Handle m =
  Handle
    { hCreateUser :: CreateUserCommand -> m CreateUserResult
    , hGenerateToken :: m SecretTokenInfo
    , hGetCurrentTime :: m UTCTime
    }

run :: Monad m => Handle m -> Query -> m (User, SecretToken)
run Handle {..} Query {..} = do
  let isAdmin = False
  tokenInfo <- hGenerateToken
  createdAt <- hGetCurrentTime
  result <-
    hCreateUser
      CreateUserCommand
        { cuFirstName = qFirstName
        , cuLastName = qLastName
        , cuAvatar = qAvatar
        , cuCreatedAt = createdAt
        , cuIsAdmin = isAdmin
        , cuTokenHash = stiHash tokenInfo
        }
  pure
    ( User
        { userId = curUserId result
        , userFirstName = qFirstName
        , userLastName = qLastName
        , userAvatarId = curAvatarId result
        , userCreatedAt = createdAt
        , userIsAdmin = isAdmin
        }
    , stiToken tokenInfo)

data Query =
  Query
      -- | The first name. This is unnecessary in case of a
      -- single-component name.
    { qFirstName :: Maybe Text
    , qLastName :: Text
    , qAvatar :: Maybe ImageQuery
    }

type ImageQuery = Image

data Image =
  Image
    { imageData :: BS.ByteString
    , imageContentType :: Text
    }
  deriving (Eq, Show)

newtype ImageId =
  ImageId
    { getImageId :: Int32
    }
  deriving (Eq, Show)

data User =
  User
  -- | The first name. This is unnecessary in case of a
  -- single-component name.
    { userId :: UserId
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

data SecretTokenInfo =
  SecretTokenInfo
    { stiToken :: SecretToken
    , stiHash :: BS.ByteString
    }

newtype SecretToken =
  SecretToken
    { secretTokenBytes :: BS.ByteString
    }
  deriving (Eq, Show)

data CreateUserCommand =
  CreateUserCommand
    { cuFirstName :: Maybe Text
    , cuLastName :: Text
    , cuAvatar :: Maybe Image
    , cuCreatedAt :: UTCTime
    , cuIsAdmin :: Bool
    , cuTokenHash :: BS.ByteString
    }

data CreateUserResult =
  CreateUserResult
    { curUserId :: UserId
    , curAvatarId :: Maybe ImageId
    }
