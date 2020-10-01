{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The web application entry point.
module Web.EntryPoint
  ( application
  , Handle(..)
  , State
  , makeState
  ) where

import Control.Exception
import Control.Exception.Sync
import Core.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.IORef
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Logger
import qualified Network.HTTP.Types as Http
import Network.HTTP.Types.Status as Http
import Web.Application
import Web.Application.Internal.SessionId
import Web.Exception
import qualified Web.Router as R

data Handle =
  Handle
    { hLogger :: Session -> Logger.Handle IO
    , hRouter :: R.Router EApplication
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

application :: Handle -> Application
application h =
  createSessionMiddleware h .
  logEnterAndExit h .
  convertExceptionsToErrorResponse h . logUncaughtExceptions h $
  routerApplication h

createSessionMiddleware :: Handle -> EApplication -> Application
createSessionMiddleware h eapp request respond = do
  session <- Session <$> generateSessionId h
  eapp session request respond

logEnterAndExit :: Handle -> EMiddleware
logEnterAndExit h eapp session@Session {..} req respond = do
  Logger.info loggerH enterMessage
  (r, status) <- runApplicationAndGetStatus (eapp session) req respond
  Logger.info loggerH (exitMessage status)
  pure r
  where
    loggerH = hLogger h session
    enterMessage =
      T.intercalate
        " "
        [ "Request"
        , requestRemoteHostAddressString req
        , T.decodeLatin1 (requestMethod req)
        , T.decodeLatin1 (requestRawPathInfo req)
        ]
    exitMessage status =
      T.intercalate
        " "
        [ "Respond"
        , T.pack (show (Http.statusCode status))
        , T.decodeLatin1 (Http.statusMessage status)
        ]

generateSessionId :: Handle -> IO SessionId
generateSessionId Handle {..} =
  atomicModifyIORef' (stSessionIdRef hState) $ \(SessionId n) ->
    let new = SessionId (succ n)
     in (new, new)

convertExceptionsToErrorResponse :: Handle -> EMiddleware
convertExceptionsToErrorResponse h eapp session request respond =
  catchJustS (exceptionToResponse h) (eapp session request respond) respond

exceptionToResponse :: Handle -> SomeException -> Maybe Response
exceptionToResponse h e
  | isAsyncException e = Nothing
  | Just webException <- fromException e =
    Just $
    case webException of
      BadRequestException reason ->
        stubErrorResponseWithReason Http.badRequest400 [] reason
      IncorrectParameterException reason ->
        stubErrorResponseWithReason Http.badRequest400 [] reason
      NotFoundException -> notFoundResponse
      UnsupportedMediaTypeException supportedTypes ->
        stubErrorResponseWithReason Http.unsupportedMediaType415 [] $
        "Supported media types are: " <> T.intercalate ", " supportedTypes
      PayloadTooLargeException maxPayloadSize ->
        stubErrorResponseWithReason Http.requestEntityTooLarge413 [] $
        "The request body length must not exceed " <>
        T.pack (show maxPayloadSize) <> " bytes"
      MalformedAuthDataException _ -> notFoundResponse
  | Just coreException <- fromException e =
    Just $
    case coreException of
      QueryException reason ->
        stubErrorResponseWithReason Http.badRequest400 [] reason
      BadCredentialsException _ -> notFoundResponse
      NoPermissionException _ _ -> notFoundResponse
      DependentEntitiesPreventDeletionException entityIdent depIds ->
        stubErrorResponseWithReason Http.badRequest400 [] $
        T.pack (show entityIdent) <>
        " cannot be deleted because the following entities depend on it: " <>
        (T.intercalate ", " . map (T.pack . show)) depIds
      EntityNotFoundException _ -> notFoundResponse
  | hShowInternalExceptionInfoInResponses h =
    Just $
    stubErrorResponseWithReason Http.internalServerError500 [] $
    "<pre>" <> T.pack (show e) <> "</pre>"
  | otherwise = Nothing

logUncaughtExceptions :: Handle -> EMiddleware
logUncaughtExceptions h eapp session request respond =
  catchJustS testException (eapp session request respond) logAndRethrow
  where
    testException e
      | isAsyncException e = Nothing
      | otherwise = Just e
    logAndRethrow e = do
      Logger.error (hLogger h session) $ T.pack (displayException e)
      throwIO e

isAsyncException :: SomeException -> Bool
isAsyncException e
  | Just (_ :: SomeAsyncException) <- fromException e = True
  | otherwise = False

routerApplication :: Handle -> EApplication
routerApplication Handle {..} session request respond =
  case R.route hRouter request of
    R.HandlerResult handler -> handler session request respond
    R.ResourceNotFoundResult -> do
      Logger.error
        (hLogger session)
        "Router failed to find a handler for the requested URL"
      respond $ stubErrorResponse Http.notFound404 []
    R.MethodNotSupportedResult knownMethods ->
      respond $ methodNotAllowedResponse knownMethods
  where
    methodNotAllowedResponse knownMethods =
      stubErrorResponse Http.methodNotAllowed405 [makeAllowHeader knownMethods]
    makeAllowHeader methods = ("Allow", B.intercalate ", " methods)

notFoundResponse :: Response
notFoundResponse = stubErrorResponse Http.notFound404 []

stubErrorResponse :: Http.Status -> [Http.Header] -> Response
stubErrorResponse status additionalHeaders =
  stubErrorResponseWithReason status additionalHeaders ""

stubErrorResponseWithReason ::
     Http.Status -> [Http.Header] -> T.Text -> Response
stubErrorResponseWithReason status additionalHeaders reason =
  responseBuilder
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
