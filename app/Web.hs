{-# LANGUAGE ScopedTypeVariables #-}

-- | The web application entry point and top-level definitions.
module Web
  ( application
  ) where

import Control.Exception
import Control.Exception.Sync
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Builder as LBS
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Router as R
import System.IO hiding (Handle)

application :: R.Router -> Wai.Application
application r =
  convertExceptionsToStatus500 $ logUncaughtExceptions $ routerApplication r

convertExceptionsToStatus500 :: Wai.Middleware
convertExceptionsToStatus500 app request respond =
  catchJustS
    testException
    (app request respond)
    (respond . Warp.defaultOnExceptionResponse)
  where
    testException e
      | Just (_ :: SomeAsyncException) <- fromException e = Nothing
      | otherwise = Just e

logUncaughtExceptions :: Wai.Middleware
logUncaughtExceptions app request respond =
  catchJustS testException (app request respond) logAndRethrow
  where
    testException e
      | Warp.defaultShouldDisplayException e = Just e
      | otherwise = Nothing
    logAndRethrow e = do
      hPutStrLn stderr (displayException e)
      throwIO e

routerApplication :: R.Router -> Wai.Application
routerApplication router request =
  case R.route router request of
    R.HandlerResult handler -> handler request
    R.ResourceNotFoundResult -> ($ stubErrorResponse Http.notFound404 [])
    R.MethodNotSupportedResult knownMethods ->
      ($ stubErrorResponse
           Http.methodNotAllowed405
           [makeAllowHeader knownMethods])
  where
    makeAllowHeader methods = ("Allow", SBS.intercalate ", " methods)

stubErrorResponse :: Http.Status -> [Http.Header] -> Wai.Response
stubErrorResponse status additionalHeaders =
  Wai.responseBuilder
    status
    ((Http.hContentType, "text/html") : additionalHeaders)
    body
  where
    body =
      mconcat
        [ "<!DOCTYPE html><html><body><h1>"
        , LBS.stringUtf8 (show (Http.statusCode status))
        , " "
        , LBS.byteString (Http.statusMessage status) <> "</h1></body></html>\n"
        ]
