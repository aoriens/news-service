{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The web application entry point.
module Web.Application
  ( application
  , Handle(..)
  , State
  , makeState
  ) where

import Control.Exception
import Control.Exception.Sync
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Builder as BB
import Data.IORef
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Logger
import qualified Network.HTTP.Types as Http
import Network.HTTP.Types.Status as Http
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Util as Wai
import Text.Printf
import qualified Web.Exception as E
import qualified Web.Router as R
import Web.Types
import Web.Types.Internal.SessionId

data Handle =
  Handle
    { hLogger :: Session -> Logger.Handle IO
    , hRouter :: R.Router
    , hState :: State
    , hShowInternalExceptionInfoInResponses :: Bool
    }

-- | A mutable state of the module. Use 'makeState' to create it and
-- store it into a 'Handle'.
newtype State =
  State
    { stSessionIdRef :: IORef SessionId
    }

-- | Creates an initial state for the module.
makeState :: IO State
makeState = do
  stSessionIdRef <- newIORef (SessionId 0)
  pure State {..}

application :: Handle -> Wai.Application
application h =
  createSessionMiddleware h .
  logEnterAndExit h .
  convertExceptionsToErrorResponse h . logUncaughtExceptions h $
  routerApplication h

createSessionMiddleware :: Handle -> EApplication -> Wai.Application
createSessionMiddleware h eapp request respond = do
  session <- Session <$> generateSessionId h
  eapp session request respond

logEnterAndExit :: Handle -> EMiddleware
logEnterAndExit h eapp session@Session {..} req respond = do
  Logger.info loggerH enterMessage
  (r, status) <- Wai.runApplicationAndGetStatus (eapp session) req respond
  Logger.info loggerH (exitMessage status)
  pure r
  where
    loggerH = hLogger h session
    enterMessage =
      T.intercalate
        " "
        [ "Request"
        , T.pack (formatPeerAddr (Wai.remoteHost req))
        , T.decodeLatin1 (Wai.requestMethod req)
        , T.decodeLatin1 (Wai.rawPathInfo req)
        ]
    exitMessage status =
      T.intercalate
        " "
        [ "Respond"
        , T.pack (show (Http.statusCode status))
        , T.decodeLatin1 (Http.statusMessage status)
        ]

-- Not so fast, though simple.
formatPeerAddr :: Socket.SockAddr -> String
formatPeerAddr (Socket.SockAddrInet _ ip4) =
  intercalate "." $ map show [b1, b2, b3, b4]
  where
    (b1, b2, b3, b4) = Socket.hostAddressToTuple ip4
formatPeerAddr (Socket.SockAddrInet6 _ _ ip6 _) =
  printf "%04X.%04X.%04X.%04X.%04X.%04X.%04X.%04X" b1 b2 b3 b4 b5 b6 b7 b8
  where
    (b1, b2, b3, b4, b5, b6, b7, b8) = Socket.hostAddress6ToTuple ip6
formatPeerAddr (Socket.SockAddrUnix s) = s

generateSessionId :: Handle -> IO SessionId
generateSessionId Handle {..} =
  atomicModifyIORef' (stSessionIdRef hState) $ \(SessionId n) ->
    let new = SessionId (succ n)
     in (new, new)

convertExceptionsToErrorResponse :: Handle -> EMiddleware
convertExceptionsToErrorResponse h eapp session request respond =
  catchJustS
    testException
    (eapp session request respond)
    (respond . exceptionToResponse h)
  where
    testException e
      | Just (_ :: SomeAsyncException) <- fromException e = Nothing
      | otherwise = Just e

exceptionToResponse :: Handle -> SomeException -> Wai.Response
exceptionToResponse h e
  | Just t@(E.BadRequestException _) <- fromException e =
    stubErrorResponseWithReason Http.badRequest400 [] $
    E.badRequestExceptionReason t
  | Just E.NotFoundException <- fromException e =
    stubErrorResponse Http.notFound404 []
  | Just t@(E.UnsupportedMediaTypeException _) <- fromException e =
    stubErrorResponseWithReason Http.unsupportedMediaType415 [] $
    "Supported media types are: " <> T.intercalate ", " (E.supportedMimeTypes t)
  | Just t@(E.PayloadTooLargeException _) <- fromException e =
    stubErrorResponseWithReason Http.requestEntityTooLarge413 [] $
    "The request body length must not exceed " <>
    T.pack (show (E.maxPayloadSize t)) <> " bytes"
  | hShowInternalExceptionInfoInResponses h = Warp.exceptionResponseForDebug e
  | otherwise = Warp.defaultOnExceptionResponse e

logUncaughtExceptions :: Handle -> EMiddleware
logUncaughtExceptions h eapp session request respond =
  catchJustS testException (eapp session request respond) logAndRethrow
  where
    testException e
      | Warp.defaultShouldDisplayException e = Just e
      | otherwise = Nothing
    logAndRethrow e = do
      Logger.error (hLogger h session) $ T.pack (displayException e)
      throwIO e

routerApplication :: Handle -> EApplication
routerApplication Handle {..} session request =
  case R.route hRouter request of
    R.HandlerResult handler -> handler session request
    R.ResourceNotFoundResult -> ($ stubErrorResponse Http.notFound404 [])
    R.MethodNotSupportedResult knownMethods ->
      ($ stubErrorResponse
           Http.methodNotAllowed405
           [makeAllowHeader knownMethods])
  where
    makeAllowHeader methods = ("Allow", SBS.intercalate ", " methods)

stubErrorResponse :: Http.Status -> [Http.Header] -> Wai.Response
stubErrorResponse status additionalHeaders =
  stubErrorResponseWithReason status additionalHeaders ""

stubErrorResponseWithReason ::
     Http.Status -> [Http.Header] -> T.Text -> Wai.Response
stubErrorResponseWithReason status additionalHeaders reason =
  Wai.responseBuilder
    status
    ((Http.hContentType, "text/html") : additionalHeaders)
    body
  where
    body =
      mconcat
        [ "<!DOCTYPE html><html><body><h1>"
        , BB.stringUtf8 (show (Http.statusCode status))
        , " "
        , BB.byteString (Http.statusMessage status)
        , "</h1><p>"
        , BB.byteString $ T.encodeUtf8 reason
        , "</p></body></html>\n"
        ]
