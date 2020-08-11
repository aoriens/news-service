{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import qualified Config as Cf
import Control.Exception
import Control.Exception.Sync
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Builder as LBS
import Data.String
import qualified Database.ConnectionManager as DBConnManager
import qualified Gateway.News
import qualified Interactor.GetNews
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Router as R
import System.IO
import qualified Web.Handler.News as HNews

-- The local environment containing configuration loaded from IO and
-- maybe some dependencies. It's purpose to be passed to pure
-- functions **in this module**, keeping extensibility in the number
-- of fields and avoiding to add excess parameters to 100500 function
-- signatures.
newtype Env =
  Env DBConnManager.Config

main :: IO ()
main = do
  config <- Cf.getConfig
  let settings = warpSettings config
      env = makeEnv config
  putStrLn "Server started"
  Warp.runSettings settings (application env)
  where
    application env =
      convertExceptionsToStatus500 $
      logUncaughtExceptions $ routerApplication env

warpSettings :: Cf.Config -> Warp.Settings
warpSettings Cf.Config {..} =
  maybe id (Warp.setServerName . fromString) cfServerName .
  maybe id (Warp.setHost . fromString) cfServerHostPreference .
  maybe id Warp.setPort cfServerPort $
  Warp.setHost "localhost" Warp.defaultSettings

makeEnv :: Cf.Config -> Env
makeEnv = Env . makeDBConnectionConfig

makeDBConnectionConfig :: Cf.Config -> DBConnManager.Config
makeDBConnectionConfig Cf.Config {..} =
  DBConnManager.makeConfig $
  (DBConnManager.connectionSettingsWithDatabaseName $ fromString cfDatabaseName)
    { DBConnManager.settingsHost = fromString <$> cfDatabaseHost
    , DBConnManager.settingsPort = cfDatabasePort
    , DBConnManager.settingsUser = fromString <$> cfDatabaseUser
    , DBConnManager.settingsPassword = fromString <$> cfDatabasePassword
    }

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

routerApplication :: Env -> Wai.Application
routerApplication env request =
  case R.route (router env) request of
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

router :: Env -> R.Router
router env =
  R.new $ do
    R.ifPath ["news"] $ do
      R.ifMethod Http.methodGet $ HNews.run (newsHandlerHandle env)

newsHandlerHandle :: Env -> HNews.Handle
newsHandlerHandle (Env dbConnectionConfig) =
  HNews.Handle
    (Interactor.GetNews.Handle
       (Gateway.News.getNews
          Gateway.News.Handle
            { Gateway.News.hWithConnection =
                DBConnManager.withConnection dbConnectionConfig
            }))
