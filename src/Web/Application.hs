module Web.Application
  ( EApplication
  , EMiddleware
  , Session(..)
  , SessionId
  , Application
  , Request(..)
  , defaultRequest
  , Response(..)
  , ResponseReceived
  , responseBuilder
  , RequestBodyLength(..)
  , responseStatusAndHeaders
  , runApplicationAndGetStatus
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.IORef
import qualified Data.Text as T
import Data.Word
import qualified Network.HTTP.Types as Http
import qualified Network.Socket
import Web.Application.Internal.ResponseReceived
import Web.Application.Internal.SessionId

type Application
   = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

data Request =
  Request
    { requestMethod :: Http.Method
    , requestHeaders :: [Http.Header]
    , pathInfo :: RequestPath
    , rawPathInfo :: B.ByteString
    , requestBodyLength :: RequestBodyLength
    , queryString :: Http.Query
    , remoteHost :: Network.Socket.SockAddr
    , strictRequestBody :: IO LB.ByteString
    , getRequestBodyChunk :: IO B.ByteString
    }

type RequestPath = [RequestPathComponent]

type RequestPathComponent = T.Text

data RequestBodyLength
  = KnownLength Word64
  | ChunkedBody

data Response =
  Response Http.Status [Http.Header] BB.Builder

type EApplication = Session -> Application

type EMiddleware = EApplication -> EApplication

-- | Shared data within single HTTP request processing.
newtype Session =
  Session
    { sessionId :: SessionId
    }

defaultRequest :: Request
defaultRequest =
  Request
    { requestMethod = Http.methodGet
    , requestHeaders = []
    , pathInfo = []
    , rawPathInfo = mempty
    , requestBodyLength = KnownLength 0
    , queryString = []
    , remoteHost = Network.Socket.SockAddrInet 0 0
    , strictRequestBody = pure mempty
    , getRequestBodyChunk = pure mempty
    }

responseBuilder :: Http.Status -> [Http.Header] -> BB.Builder -> Response
responseBuilder = Response

responseStatusAndHeaders :: Response -> (Http.Status, [Http.Header])
responseStatusAndHeaders (Response status headers _) = (status, headers)

-- | Runs the application and returns its HTTP status after finish.
runApplicationAndGetStatus ::
     Application
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO (ResponseReceived, Http.Status)
runApplicationAndGetStatus app request respond = do
  statusRef <- newIORef (error "The response status must be set here")
  r <-
    app request $ \response -> do
      let (status, _) = responseStatusAndHeaders response
      writeIORef statusRef status
      respond response
  status <- readIORef statusRef
  pure (r, status)
