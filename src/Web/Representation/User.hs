{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.User
  ( UserRep
  , userRep
  ) where

import Core.Authentication as Core
import Core.User
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Web.AppURI as U
import Web.Credentials
import Web.RepresentationBuilder

data UserRep =
  UserRep
    { userUserId :: Int32
    , userFirstName :: Maybe T.Text
    , userLastName :: T.Text
    , userAvatarURL :: Maybe AppURIRep
    , userCreatedAt :: UTCTime
    , userIsAdmin :: Bool
    , userSecretToken :: Maybe T.Text
    }

userRep :: Maybe Core.Credentials -> User -> RepBuilder UserRep
userRep creds User {..} = do
  avatarURL <- renderMaybeAppURI (U.ImageURI <$> userAvatarId)
  pure
    UserRep
      { userUserId = getUserId userId
      , userFirstName = userFirstName
      , userLastName = userLastName
      , userAvatarURL = avatarURL
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
    ''UserRep)
