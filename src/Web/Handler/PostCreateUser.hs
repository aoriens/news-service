{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.PostCreateUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Control.Monad
import qualified Core.Interactor.CreateUser as I
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DL
import Data.Int
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Word
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Util as Wai
import Web.Exception

run :: Handle -> Wai.Application
run h request respond =
  respond $
  Wai.simpleResponseStream
    Http.ok200
    [(Http.hContentType, "application/json")]
    (buildResponse h request)

data Handle =
  Handle
    { hCreateUserHandle :: I.Handle IO
    , hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    }

buildResponse :: Handle -> Wai.Request -> IO BB.Builder
buildResponse Handle {..} request = do
  rejectInvalidContentType request
  bodyBytes <- loadRequestBodyNoLonger maxBodyLength request
  userEntity <-
    either (throwIO . BadRequestException . T.pack) pure $
    A.eitherDecode' bodyBytes
  (user, secretToken) <- I.run hCreateUserHandle (queryFromInUser userEntity)
  pure $ hJSONEncode (formatResponse user secretToken)

rejectInvalidContentType :: Wai.Request -> IO ()
rejectInvalidContentType request =
  when
    ((Http.hContentType, expectedContentType) `notElem`
     Wai.requestHeaders request) $
  throwIO (UnsupportedMediaTypeException [T.decodeUtf8 expectedContentType])

expectedContentType :: BS.ByteString
expectedContentType = "application/json"

loadRequestBodyNoLonger :: Word64 -> Wai.Request -> IO LBS.ByteString
loadRequestBodyNoLonger maxLen request =
  case Wai.requestBodyLength request of
    Wai.KnownLength len
      | len > maxLen -> throwIO $ PayloadTooLargeException maxLen
      | otherwise -> Wai.strictRequestBody request
    Wai.ChunkedBody -> go 0 DL.empty
  where
    go len chunks
      | len > maxLen = throwIO $ PayloadTooLargeException maxLen
      | otherwise = do
        chunk <- Wai.getRequestBodyChunk request
        if BS.null chunk
          then pure $ LBS.fromChunks (DL.toList chunks)
          else go
                 (len + fromIntegral (BS.length chunk))
                 (chunks `DL.snoc` chunk)

maxBodyLength :: Word64
maxBodyLength = 65536

queryFromInUser :: InUser -> I.Query
queryFromInUser InUser {..} =
  I.Query
    { qFirstName = iuFirstName
    , qLastName = iuLastName
    , qAvatar = imageFromInImage <$> iuAvatar
    }

imageFromInImage :: InImage -> I.Image
imageFromInImage InImage {..} =
  I.Image {imageData = unBase64 iiBase64Data, imageContentType = iiContentType}

formatResponse :: I.User -> I.SecretToken -> OutUser
formatResponse I.User {..} (I.SecretToken tokenBytes) =
  OutUser
    { ouId = I.getUserId userId
    , ouFirstName = userFirstName
    , ouLastName = userLastName
    , ouAvatarId = I.getImageId <$> userAvatarId
    , ouCreatedAt = userCreatedAt
    , ouIsAdmin = userIsAdmin
    , ouSecretToken = Base64 tokenBytes
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

data OutUser =
  OutUser
    { ouId :: Int32
    , ouFirstName :: Maybe Text
    , ouLastName :: Text
    , ouAvatarId :: Maybe Int32
    , ouCreatedAt :: UTCTime
    , ouIsAdmin :: Bool
    , ouSecretToken :: Base64
    }

newtype Base64 =
  Base64
    { unBase64 :: BS.ByteString
    }

instance A.ToJSON Base64 where
  toJSON = A.String . B64.encodeBase64 . unBase64
  toEncoding = A.text . B64.encodeBase64 . unBase64

instance A.FromJSON Base64 where
  parseJSON =
    A.withText "base64 string" $
    either (fail . T.unpack) (pure . Base64) . B64.decodeBase64 . T.encodeUtf8

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "iu"}
    ''InUser)

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "ii"}
    ''InImage)

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "ou"}
    ''OutUser)
