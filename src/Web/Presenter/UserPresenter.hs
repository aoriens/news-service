{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Presenter.UserPresenter
  ( presentUser
  , presentUsers
  , Handle(..)
  ) where

import qualified Core.Authentication as Auth
import qualified Core.User as C
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Builder as BB
import Data.Int
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Web.AppURL as U
import Web.Credentials

data Handle =
  Handle
    { hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hRenderAppURL :: U.AppURL -> Text
    }

presentUser :: Handle -> C.User -> Maybe Auth.Credentials -> BB.Builder
presentUser h user creds = hJSONEncode h $ userEntity h creds user

presentUsers :: Handle -> [C.User] -> BB.Builder
presentUsers h = hJSONEncode h . map (userEntity h Nothing)

userEntity :: Handle -> Maybe Auth.Credentials -> C.User -> User
userEntity h creds C.User {..} =
  User
    { userId = C.getUserId userId
    , userFirstName = userFirstName
    , userLastName = userLastName
    , userAvatarURL = hRenderAppURL h . U.URLImage <$> userAvatarId
    , userCreatedAt = userCreatedAt
    , userIsAdmin = userIsAdmin
    , userSecretToken =
        T.decodeLatin1 . unWebToken . presentCredentials <$> creds
    }

data User =
  User
    { userId :: Int32
    , userFirstName :: Maybe Text
    , userLastName :: Text
    , userAvatarURL :: Maybe Text
    , userCreatedAt :: UTCTime
    , userIsAdmin :: Bool
    , userSecretToken :: Maybe Text
    }

$(A.deriveToJSON
    A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "user"
      , A.omitNothingFields = True
      }
    ''User)
