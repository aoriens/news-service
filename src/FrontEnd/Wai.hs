module FrontEnd.Wai
  ( toWaiApplication
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.DList as DL
import Data.IORef
import Data.Word
import qualified Network.Wai as Wai
import Web.Application
import Web.Application.Internal.ResponseReceived

toWaiApplication :: Application -> Wai.Application
toWaiApplication app waiRequest waiSendResponse = do
  ref <- newIORef $ error "Response continuation must have been invoked"
  ResponseReceived <-
    app (fromWaiRequest waiRequest) $ \response -> do
      r <- waiSendResponse $ toWaiResponse response
      writeIORef ref r
      pure ResponseReceived
  readIORef ref

fromWaiRequest :: Wai.Request -> Request
fromWaiRequest r =
  Request
    { requestMethod = Wai.requestMethod r
    , requestHeaders = Wai.requestHeaders r
    , requestPathInfo = Wai.pathInfo r
    , requestRawPathInfo = Wai.rawPathInfo r
    , requestQueryString = Wai.queryString r
    , remoteHost = Wai.remoteHost r
    , requestLoadBodyNoLonger = loadRequestBodyNoLonger r
    }

loadRequestBodyNoLonger :: Wai.Request -> Word64 -> IO (Maybe LB.ByteString)
loadRequestBodyNoLonger request maxLen =
  case Wai.requestBodyLength request of
    Wai.KnownLength len
      | len > maxLen -> pure Nothing
      | otherwise -> Just <$> Wai.strictRequestBody request
    Wai.ChunkedBody -> go 0 DL.empty
  where
    go len chunks
      | len > maxLen = pure Nothing
      | otherwise = do
        chunk <- Wai.getRequestBodyChunk request
        if B.null chunk
          then pure . Just $ LB.fromChunks (DL.toList chunks)
          else go (len + fromIntegral (B.length chunk)) (chunks `DL.snoc` chunk)

toWaiResponse :: Response -> Wai.Response
toWaiResponse (Response status headers builder) =
  Wai.responseBuilder status headers builder
