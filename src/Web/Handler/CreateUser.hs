{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.CreateUser
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import Core.Image
import qualified Core.Interactor.CreateUser as I
import Core.User
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import Data.Text (Text)
import Web.Representation.Base64
import Web.Types

data Handle =
  Handle
    { hCreateUserHandle :: I.Handle IO
    , hPresenter :: User -> Credentials -> Response
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Request -> IO a
    }

run :: Handle -> Application
run Handle {..} request respond = do
  userEntity <- hLoadJSONRequestBody request
  (user, credentials) <- I.run hCreateUserHandle (queryFromInUser userEntity)
  respond $ hPresenter user credentials

queryFromInUser :: InUser -> I.Query
queryFromInUser InUser {..} =
  I.Query
    { qFirstName = iuFirstName
    , qLastName = iuLastName
    , qAvatar = imageQueryFromInImage <$> iuAvatar
    }

imageQueryFromInImage :: InImage -> I.ImageQuery
imageQueryFromInImage InImage {..} =
  Image {imageData = unBase64 iiBase64Data, imageContentType = iiContentType}

data InUser =
  InUser
    { iuFirstName :: Maybe Text
    , iuLastName :: Text
    , iuAvatar :: Maybe InImage
    }

data InImage =
  InImage
    { iiBase64Data :: Base64
    , iiContentType :: Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "iu"}
    ''InUser)

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "ii"}
    ''InImage)
