{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.User
  ( User
  , userRepresentation
  ) where

import Core.Authentication as Core
import qualified Core.User as Core
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Web.AppURL as U
import Web.Credentials

data User =
  User
    { userId :: Int32
    , userFirstName :: Maybe T.Text
    , userLastName :: T.Text
    , userAvatarURL :: Maybe T.Text
    , userCreatedAt :: UTCTime
    , userIsAdmin :: Bool
    , userSecretToken :: Maybe T.Text
    }

userRepresentation ::
     (U.AppURL -> T.Text) -> Maybe Core.Credentials -> Core.User -> User
userRepresentation renderAppURL creds Core.User {..} =
  User
    { userId = Core.getUserId userId
    , userFirstName = userFirstName
    , userLastName = userLastName
    , userAvatarURL = renderAppURL . U.URLImage <$> userAvatarId
    , userCreatedAt = userCreatedAt
    , userIsAdmin = userIsAdmin
    , userSecretToken =
        T.decodeLatin1 . unWebToken . presentCredentials <$> creds
    }

$(A.deriveToJSON
    A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "user"
      , A.omitNothingFields = True
      }
    ''User)
