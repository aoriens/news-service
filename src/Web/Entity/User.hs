{-# LANGUAGE TemplateHaskell #-}

module Web.Entity.User
  ( User(..)
  , Base64(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Web.Entity.Base64

data User =
  User
    { userId :: Int32
    , userFirstName :: Maybe Text
    , userLastName :: Text
    , userAvatarURL :: Maybe Text
    , userCreatedAt :: UTCTime
    , userIsAdmin :: Bool
    , userSecretToken :: Base64
    }

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "user"}
    ''User)
