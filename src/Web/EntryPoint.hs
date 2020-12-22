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
import Data.IORef
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Show
import qualified Logger
import Network.HTTP.Types.Status as Http
import Web.Application
import Web.Application.Internal.SessionId
import Web.Exception
import qualified Web.Router as R

data Handle =
  Handle
    { hLogger :: Session -> Logger.Handle IO
    , hRouter :: R.Router ApplicationWithSession
    , hState :: State
    , hShowInternalExceptionInfoInResponses :: Bool
    , hPresentCoreException :: CoreException -> Response
    , hPresentWebException :: WebException -> Response
    , hNotFoundResponse :: Response
    , hMethodNotAllowedResponse :: [B.ByteString] -> Response
    -- ^ Accepts the supported methods, unordered.
    , hUncaughtExceptionResponseForDebug :: SomeException -> Response
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

createSessionMiddleware :: Handle -> ApplicationWithSession -> Application
createSessionMiddleware h eapp request respond = do
  session <- Session <$> generateSessionId h
  eapp session request respond

logEnterAndExit :: Handle -> MiddlewareWithSession
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
        , showAsText (Http.statusCode status)
        , T.decodeLatin1 (Http.statusMessage status)
        ]

generateSessionId :: Handle -> IO SessionId
generateSessionId Handle {..} =
  atomicModifyIORef' (stSessionIdRef hState) $ \(SessionId n) ->
    let new = SessionId (succ n)
     in (new, new)

convertExceptionsToErrorResponse :: Handle -> MiddlewareWithSession
convertExceptionsToErrorResponse h eapp session request respond =
  catchJustS (exceptionToResponse h) (eapp session request respond) respond

exceptionToResponse :: Handle -> SomeException -> Maybe Response
exceptionToResponse h e
  | isAsyncException e = Nothing
  | Just webException <- fromException e =
    Just $ hPresentWebException h webException
  | Just coreException <- fromException e =
    Just $ hPresentCoreException h coreException
  | hShowInternalExceptionInfoInResponses h =
    Just $ hUncaughtExceptionResponseForDebug h e
  | otherwise = Nothing

logUncaughtExceptions :: Handle -> MiddlewareWithSession
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

routerApplication :: Handle -> ApplicationWithSession
routerApplication Handle {..} session request respond =
  case R.route hRouter request of
    R.HandlerResult handler -> handler session request respond
    R.ResourceNotFoundResult -> do
      Logger.error
        (hLogger session)
        "Router failed to find a handler for the requested URL"
      respond hNotFoundResponse
    R.MethodNotSupportedResult knownMethods ->
      respond $ hMethodNotAllowedResponse knownMethods
