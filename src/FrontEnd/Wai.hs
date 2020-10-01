module FrontEnd.Wai
  ( toWaiApplication
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.DList as DL
import Data.IORef
import Data.List
import qualified Data.Text as T
import Data.Word
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import Text.Printf
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
    , requestRemoteHostAddressString = formatPeerAddr $ Wai.remoteHost r
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

-- Not so fast, though simple.
formatPeerAddr :: Socket.SockAddr -> T.Text
formatPeerAddr addr =
  T.pack $
  case addr of
    Socket.SockAddrInet _ ip4 ->
      let (b1, b2, b3, b4) = Socket.hostAddressToTuple ip4
       in intercalate "." $ map show [b1, b2, b3, b4]
    Socket.SockAddrInet6 _ _ ip6 _ ->
      let (b1, b2, b3, b4, b5, b6, b7, b8) = Socket.hostAddress6ToTuple ip6
          format = "%04X:%04X:%04X:%04X:%04X:%04X:%04X:%04X"
       in printf format b1 b2 b3 b4 b5 b6 b7 b8
    Socket.SockAddrUnix s -> s

toWaiResponse :: Response -> Wai.Response
toWaiResponse (Response status headers builder) =
  Wai.responseBuilder status headers builder
