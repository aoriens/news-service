{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Presenter.UserPresenter
  ( presentUser
  , presentUsers
  , Handle(..)
  ) where

import qualified Core.DTO.User as C
import qualified Core.Interactor.CreateUser as I
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Builder as BB
import Data.Int
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time
import qualified Web.AppURL as U
import Web.Entity.Base64

data Handle =
  Handle
    { hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hRenderAppURL :: U.AppURL -> Text
    }

presentUser :: Handle -> C.User -> Maybe I.SecretToken -> BB.Builder
presentUser h user token = hJSONEncode h $ userEntity h token user

presentUsers :: Handle -> [C.User] -> BB.Builder
presentUsers h = hJSONEncode h . map (userEntity h Nothing)

userEntity :: Handle -> Maybe I.SecretToken -> C.User -> User
userEntity h token C.User {..} =
  User
    { userId = C.getUserId userId
    , userFirstName = userFirstName
    , userLastName = userLastName
    , userAvatarURL = hRenderAppURL h . U.URLImage <$> userAvatarId
    , userCreatedAt = userCreatedAt
    , userIsAdmin = userIsAdmin
    , userSecretToken = Base64 . I.secretTokenBytes <$> token
    }

data User =
  User
    { userId :: Int32
    , userFirstName :: Maybe Text
    , userLastName :: Text
    , userAvatarURL :: Maybe Text
    , userCreatedAt :: UTCTime
    , userIsAdmin :: Bool
    , userSecretToken :: Maybe Base64
    }

$(A.deriveToJSON
    A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "user"
      , A.omitNothingFields = True
      }
    ''User)
