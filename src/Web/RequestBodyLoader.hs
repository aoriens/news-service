-- | A recommended way to get request bodies.
module Web.RequestBodyLoader
  ( loadJSONRequestBody
  , Config(..)
  ) where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Web.Exception as E
import qualified Web.HTTP as Http

newtype Config =
  Config
    { cfMaxBodySize :: Word64
    }

-- | Loads and decodes JSON request body. It can perform checks for
-- Content-Type header, for excess body length etc. It can throw
-- exceptions from 'Web.Exception' module.
loadJSONRequestBody :: A.FromJSON a => Config -> Wai.Request -> IO a
loadJSONRequestBody config request = do
  rejectInvalidContentType request
  body <- loadRequestBodyNoLonger (cfMaxBodySize config) request
  either (throwIO . E.BadRequestException . T.pack) pure $ A.eitherDecode' body

rejectInvalidContentType :: Wai.Request -> IO ()
rejectInvalidContentType request =
  when
    ((Http.hContentType, expectedContentType) `notElem`
     Wai.requestHeaders request) $
  throwIO (E.UnsupportedMediaTypeException [T.decodeLatin1 expectedContentType])

expectedContentType :: BS.ByteString
expectedContentType = Http.jsonContentType

loadRequestBodyNoLonger :: Word64 -> Wai.Request -> IO LBS.ByteString
loadRequestBodyNoLonger maxLen request =
  case Wai.requestBodyLength request of
    Wai.KnownLength len
      | len > maxLen -> throwIO $ E.PayloadTooLargeException maxLen
      | otherwise -> Wai.strictRequestBody request
    Wai.ChunkedBody -> go 0 DL.empty
  where
    go len chunks
      | len > maxLen = throwIO $ E.PayloadTooLargeException maxLen
      | otherwise = do
        chunk <- Wai.getRequestBodyChunk request
        if BS.null chunk
          then pure $ LBS.fromChunks (DL.toList chunks)
          else go
                 (len + fromIntegral (BS.length chunk))
                 (chunks `DL.snoc` chunk)
