module FrontEnd.Wai
  ( toWaiApplication
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.DList as DL
import Data.IORef
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
    , pathInfo = Wai.pathInfo r
    , rawPathInfo = Wai.rawPathInfo r
    , requestBodyLength =
        case Wai.requestBodyLength r of
          Wai.KnownLength s -> KnownLength s
          Wai.ChunkedBody -> ChunkedBody
    , queryString = Wai.queryString r
    , remoteHost = Wai.remoteHost r
    , getRequestBodyChunk = Wai.getRequestBodyChunk r
    , strictRequestBody = loadFullRequestBody $ Wai.getRequestBodyChunk r
    }

loadFullRequestBody :: IO B.ByteString -> IO LB.ByteString
loadFullRequestBody loadChunk = go DL.empty
  where
    go chunks = do
      chunk <- loadChunk
      if B.null chunk
        then pure $ LB.fromChunks $ DL.toList chunks
        else go (chunks `DL.snoc` chunk)

toWaiResponse :: Response -> Wai.Response
toWaiResponse (Response status headers builder) =
  Wai.responseBuilder status headers builder
