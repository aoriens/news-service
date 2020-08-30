{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.PostCreateUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.DTO.Image
import Core.Exception
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
import Web.Entity.Base64
import Web.Exception
import qualified Web.HTTP as Http
import qualified Web.Presenter.UserPresenter as P

data Handle =
  Handle
    { hCreateUserHandle :: I.Handle IO
    , hPresenterHandle :: P.Handle
    , hGetRequestBody :: Wai.Request -> IO LBS.ByteString
    }

run :: Handle -> Wai.Application
run h request respond = do
  builder <- buildResponse h request
  respond $ Wai.responseBuilder Http.ok200 [Http.hJSONContentType] builder

buildResponse :: Handle -> Wai.Request -> IO BB.Builder
buildResponse Handle {..} request = do
  bodyBytes <- hGetRequestBody request
  userEntity <-
    either (throwIO . BadRequestException . T.pack) pure $
    A.eitherDecode' bodyBytes
  (user, secretToken) <-
    catch
      (I.run hCreateUserHandle (queryFromInUser userEntity))
      (throwIO . BadRequestException . queryExceptionReason)
  pure $ P.presentUser hPresenterHandle user (Just secretToken)

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
