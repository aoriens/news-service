{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Exception
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Builder as LBS
import qualified Database.ConnectionManager as DBConnManager
import qualified Gateway.News
import qualified Handler.News
import qualified Interactor.GetNews
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Router as R
import System.IO

main :: IO ()
main = do
  putStrLn "Server started"
  Warp.run 4000 . convertExceptionsToStatus500 . logUncaughtExceptions $
    routerApplication

convertExceptionsToStatus500 :: Wai.Middleware
convertExceptionsToStatus500 app request respond =
  tryJust testException (app request respond) >>=
  either (respond . Warp.defaultOnExceptionResponse) pure
  where
    testException e
      | Just (_ :: SomeAsyncException) <- fromException e = Nothing
      | otherwise = Just e

logUncaughtExceptions :: Wai.Middleware
logUncaughtExceptions app request respond =
  tryJust testException (app request respond) >>= either logAndRethrow pure
  where
    testException e
      | Warp.defaultShouldDisplayException e = Just e
      | otherwise = Nothing
    logAndRethrow e = do
      hPutStrLn stderr (displayException e)
      throwIO e

routerApplication :: Wai.Application
routerApplication request =
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

router :: R.Router
router =
  R.new $ do
    R.ifPath ["news"] $ do
      R.ifMethod Http.methodGet $ Handler.News.run newsHandlerHandle

newsHandlerHandle :: Handler.News.Handle
newsHandlerHandle =
  Handler.News.Handle
    (Interactor.GetNews.Handle
       (Gateway.News.getNews
          Gateway.News.Handle
            { Gateway.News.hWithConnection =
                DBConnManager.withConnection dbConnectionConfig
            }))

dbConnectionConfig :: DBConnManager.Config
dbConnectionConfig =
  DBConnManager.makeConfig
    (DBConnManager.connectionSettingsWithDatabaseName "news")
