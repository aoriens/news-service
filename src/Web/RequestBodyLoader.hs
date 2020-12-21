-- | A recommended way to get request bodies.
module Web.RequestBodyLoader
  ( loadJSONRequestBody
  , Config(..)
  ) where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe.Util
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import qualified Network.HTTP.Types as Http
import Web.Application
import qualified Web.Exception as E
import qualified Web.HTTP as Http

newtype Config =
  Config
    { cfMaxBodySize :: Word64
    }

-- | Loads and decodes JSON request body. It can perform checks for
-- Content-Type header, for excess body length etc. It can throw
-- exceptions from 'Web.Exception' module.
loadJSONRequestBody :: A.FromJSON a => Config -> Request -> IO a
loadJSONRequestBody config request = do
  rejectInvalidContentType request
  body <- loadRequestBodyNoLonger (cfMaxBodySize config) request
  either throwException pure $ A.eitherDecode' body
  where
    throwException =
      throwIO .
      E.BadRequestException . ("When decoding JSON request body: " <>) . T.pack

rejectInvalidContentType :: Request -> IO ()
rejectInvalidContentType request =
  when
    ((Http.hContentType, expectedContentType) `notElem` requestHeaders request) $
  throwIO (E.UnsupportedMediaTypeException [T.decodeLatin1 expectedContentType])

expectedContentType :: B.ByteString
expectedContentType = Http.jsonContentType

loadRequestBodyNoLonger :: Word64 -> Request -> IO LB.ByteString
loadRequestBodyNoLonger maxLen request =
  requestLoadBodyNoLonger request maxLen >>=
  fromMaybeM (throwIO $ E.PayloadTooLargeException maxLen)
