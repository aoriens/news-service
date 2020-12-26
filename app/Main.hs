{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  ) where

import Config
import qualified Config.IO
import Control.Concurrent.Async
import Control.Exception
import Control.Exception.Sync
import Control.Monad.IO.Class
import qualified Core.Authentication
import qualified Core.Authentication.Impl
import qualified Core.Pagination.Impl
import Data.Text.Show
import qualified Database
import qualified Database.Service.Primitives as Database
import qualified FrontEnd.Wai
import qualified Gateway.SecretToken as GSecretToken
import qualified Handlers
import qualified Logger
import qualified Logger.Impl
import qualified Network.Wai.Handler.Warp as Warp
import System.Exit
import System.IO hiding (Handle)
import Web.AppURI
import qualified Web.Application as Web
import qualified Web.EntryPoint
import qualified Web.JSONEncoder as JSONEncoder
import Web.Presenter.Error
import Web.RepresentationBuilder
import qualified Web.RequestBodyLoader as RequestBodyLoader

-- Common dependencies, built using IO-actions. They are placed here,
-- so that they could be created once and easily passed to other
-- functions.
data Deps =
  Deps
    { dConfig :: Config
    , dLoggerHandle :: Logger.Handle IO
    , dSecretTokenIOState :: GSecretToken.IOState
    }

main :: IO ()
main = do
  (loggerWorker, deps@Deps {..}) <- getDeps
  webHandle <- getWebEntryPointHandle deps
  race_ loggerWorker $ do
    Logger.info dLoggerHandle "Starting Warp"
    Warp.runSettings
      (cfWarpSettings dConfig)
      (FrontEnd.Wai.toWaiApplication $ Web.EntryPoint.application webHandle)

getDeps :: IO (Logger.Impl.Worker, Deps)
getDeps = do
  dConfig <- loadConfig
  (loggerWorker, dLoggerHandle) <- getLoggerHandle dConfig
  dSecretTokenIOState <- GSecretToken.initIOState
  pure (loggerWorker, Deps {dConfig, dLoggerHandle, dSecretTokenIOState})

-- | Creates an IO action and a logger handle. The IO action must be
-- forked in order for logging to work.
getLoggerHandle :: Config -> IO (Logger.Impl.Worker, Logger.Handle IO)
getLoggerHandle Config {..} = do
  hFileHandle <- getFileHandle cfLogFile
  Logger.Impl.new
    Logger.Impl.Handle {hFileHandle, hMinLevel = cfLoggerVerbosity}
  where
    getFileHandle (LogFilePath path) =
      openFile path AppendMode `catchS` \e ->
        die $ "While opening log file: " ++ displayException (e :: IOException)
    getFileHandle LogFileStdErr = pure stderr

loadConfig :: IO Config
loadConfig = do
  inConfig <- Config.IO.getConfig
  either die pure $ makeConfig inConfig

getWebEntryPointHandle :: Deps -> IO Web.EntryPoint.Handle
getWebEntryPointHandle deps@Deps {..} = do
  hState <- Web.EntryPoint.makeState
  pure
    Web.EntryPoint.Handle
      { hState
      , hLogger = (`sessionLoggerHandle` dLoggerHandle)
      , hHandlers = injectDependenciesToHandler deps <$> Handlers.handlers
      , hShowInternalExceptionInfoInResponses =
          cfShowInternalErrorInfoInResponse dConfig
      , hPresentCoreException =
          presentCoreException $ representationBuilderHandleWith dConfig
      , hPresentWebException =
          presentWebException $ representationBuilderHandleWith dConfig
      , hNotFoundResponse = notFoundResponse
      , hMethodNotAllowedResponse = methodNotAllowedResponse
      , hUncaughtExceptionResponseForDebug = uncaughtExceptionResponseForDebug
      }

sessionLoggerHandle :: Web.Session -> Logger.Handle IO -> Logger.Handle IO
sessionLoggerHandle Web.Session {..} =
  Logger.mapMessage $ \text -> "SID-" <> showAsText sessionId <> " " <> text

representationBuilderHandleWith :: Config -> RepBuilderHandle
representationBuilderHandleWith config =
  RepBuilderHandle
    { hJSONEncode =
        JSONEncoder.encode
          JSONEncoder.Config {prettyPrint = cfJSONPrettyPrint config}
    , hRenderAppURI = Web.AppURI.renderAppURI $ cfAppURIConfig config
    }

injectDependenciesToHandler ::
     Deps -> Handlers.Handler -> Web.ApplicationWithSession
injectDependenciesToHandler deps handler session =
  transactionApplicationToIOApplication (databaseHandleWith deps session) $
  handler $ handlersDepsWith deps session

transactionApplicationToIOApplication ::
     Database.Handle
  -> Web.GenericApplication Database.Transaction
  -> Web.GenericApplication IO
transactionApplicationToIOApplication h =
  Web.mapGenericApplication (Database.runTransactionRW h) liftIO

databaseHandleWith :: Deps -> Web.Session -> Database.Handle
databaseHandleWith Deps {..} session =
  Database.Handle
    { hConnectionConfig = cfDatabaseConfig dConfig
    , hLoggerHandle = sessionLoggerHandle session dLoggerHandle
    }

handlersDepsWith :: Deps -> Web.Session -> Handlers.Deps
handlersDepsWith deps@Deps {..} session =
  Handlers.Deps
    { dConfig
    , dPageSpecParserHandle = Core.Pagination.Impl.new $ cfMaxPageLimit dConfig
    , dLoadJSONRequestBody =
        liftIO .
        RequestBodyLoader.loadJSONRequestBody
          RequestBodyLoader.Config
            {cfMaxBodySize = cfMaxRequestJsonBodySize dConfig}
    , dSecretTokenIOState
    , dAppURIConfig = cfAppURIConfig dConfig
    , dRepresentationBuilderHandle = representationBuilderHandleWith dConfig
    , dAuthenticate =
        Core.Authentication.authenticate $ authenticationHandleWith deps session
    }

authenticationHandleWith ::
     Deps
  -> Web.Session
  -> Core.Authentication.AuthenticationHandle Database.Transaction
authenticationHandleWith Deps {..} session =
  Core.Authentication.Impl.new
    Core.Authentication.Impl.Handle
      { hGetUserAuthData = Database.getUserAuthData
      , hTokenMatchesHash = GSecretToken.tokenMatchesHash
      , hLoggerHandle =
          Logger.mapHandle liftIO $ sessionLoggerHandle session dLoggerHandle
      }
