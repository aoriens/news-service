{-# LANGUAGE RankNTypes #-}

module Web.Application
  ( ApplicationWithSession
  , MiddlewareWithSession
  , Session(..)
  , SessionId
  , Application
  , GenericApplication
  , mapGenericApplication
  , Request(..)
  , defaultRequest
  , Response(..)
  , ResponseReceived
  , responseWithBuilder
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
import Web.Application.Internal.ResponseReceived
import Web.Application.Internal.SessionId

type Application = GenericApplication IO

type GenericApplication m
   = Request -> (Response -> m ResponseReceived) -> m ResponseReceived

-- | Converts a 'GenericApplication' within a monad into one in a
-- different monad.
mapGenericApplication ::
     (forall a. m1 a -> m2 a)
  -> (forall a. m2 a -> m1 a)
  -> GenericApplication m1
  -> GenericApplication m2
mapGenericApplication mapForward mapBackward app1 request respond2 =
  mapForward $ app1 request (mapBackward . respond2)

data Request =
  Request
    { requestMethod :: Http.Method
    , requestHeaders :: [Http.Header]
    , requestPathInfo :: RequestPath
    , requestRawPathInfo :: B.ByteString
    , requestQueryString :: Http.Query
    , requestRemoteHostAddressString :: T.Text
    , requestLoadBodyNoLonger :: Word64 -> IO (Maybe LB.ByteString)
    -- ^ Loads the request body no longer than the specified amount of
    -- octets. Returns Nothing in case of a too large body.
    }

type RequestPath = [RequestPathComponent]

type RequestPathComponent = T.Text

data Response =
  Response Http.Status [Http.Header] BB.Builder

type ApplicationWithSession = Session -> Application

type MiddlewareWithSession = ApplicationWithSession -> ApplicationWithSession

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
    , requestPathInfo = []
    , requestRawPathInfo = mempty
    , requestQueryString = []
    , requestRemoteHostAddressString = "<localhost>"
    , requestLoadBodyNoLonger = \_ -> pure $ Just mempty
    }

responseWithBuilder :: Http.Status -> [Http.Header] -> BB.Builder -> Response
responseWithBuilder = Response

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
