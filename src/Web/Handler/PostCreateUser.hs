{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.PostCreateUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.CreateUser as I
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.AppURL as U
import Web.Entity.User
import Web.Exception

data Handle =
  Handle
    { hCreateUserHandle :: I.Handle IO
    , hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hGetRequestBody :: Wai.Request -> IO LBS.ByteString
    , hRenderAppURL :: U.AppURL -> Text
    }

run :: Handle -> Wai.Application
run h request respond = do
  builder <- buildResponse h request
  respond $
    Wai.responseBuilder
      Http.ok200
      [(Http.hContentType, "application/json")]
      builder

buildResponse :: Handle -> Wai.Request -> IO BB.Builder
buildResponse h@Handle {..} request = do
  bodyBytes <- hGetRequestBody request
  userEntity <-
    either (throwIO . BadRequestException . T.pack) pure $
    A.eitherDecode' bodyBytes
  (user, secretToken) <-
    catch
      (I.run hCreateUserHandle (queryFromInUser userEntity))
      (throwIO . BadRequestException . I.queryExceptionReason)
  pure $ hJSONEncode (formatResponse h user secretToken)

queryFromInUser :: InUser -> I.Query
queryFromInUser InUser {..} =
  I.Query
    { qFirstName = iuFirstName
    , qLastName = iuLastName
    , qAvatar = imageQueryFromInImage <$> iuAvatar
    }

imageQueryFromInImage :: InImage -> I.ImageQuery
imageQueryFromInImage InImage {..} =
  I.Image {imageData = unBase64 iiBase64Data, imageContentType = iiContentType}

formatResponse :: Handle -> I.User -> I.SecretToken -> User
formatResponse Handle {..} I.User {..} (I.SecretToken tokenBytes) =
  User
    { userId = I.getUserId userId
    , userFirstName = userFirstName
    , userLastName = userLastName
    , userAvatarURL = hRenderAppURL . U.URLImage <$> userAvatarId
    , userCreatedAt = userCreatedAt
    , userIsAdmin = userIsAdmin
    , userSecretToken = Base64 tokenBytes
    }

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
