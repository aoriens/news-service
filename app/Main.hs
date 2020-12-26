{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  ) where

import qualified Config as Cf
import qualified Config.IO as CIO
import Control.Concurrent.Async
import Control.Exception
import Control.Exception.Sync
import Control.Monad.IO.Class
import qualified Core.Authentication
import qualified Core.Authentication.Impl
import Core.Pagination
import qualified Core.Pagination.Impl
import qualified Data.Aeson as A
import Data.Text.Show
import qualified Database
import qualified Database.Service.ConnectionManager as DBConnManager
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

-- Some common module dependencies. Its purpose is to be passed to
-- functions **in this module**, keeping extensibility in the number
-- of fields and avoiding to add excess parameters to 100500 function
-- signatures.
data Deps =
  Deps
    { dDatabaseConnectionConfig :: DBConnManager.Config
    , dConfig :: Cf.Config
    , dLoggerHandle :: Logger.Handle IO
    , dPageSpecParserHandle :: PageSpecParserHandle
    , dLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Web.Request -> Database.Transaction a
    , dSecretTokenIOState :: GSecretToken.IOState
    , dAppURIConfig :: AppURIConfig
    , dRepresentationBuilderHandle :: RepBuilderHandle
    }

main :: IO ()
main = do
  (loggerWorker, deps@Deps {..}) <- getDeps
  webHandle <- getWebEntryPointHandle deps
  race_ loggerWorker $ do
    Logger.info dLoggerHandle "Starting Warp"
    Warp.runSettings
      (Cf.cfWarpSettings dConfig)
      (FrontEnd.Wai.toWaiApplication $ Web.EntryPoint.application webHandle)

getDeps :: IO (Logger.Impl.Worker, Deps)
getDeps = do
  inConfig <- CIO.getConfig
  dConfig <- either die pure $ Cf.makeConfig inConfig
  (loggerWorker, dLoggerHandle) <- getLoggerHandle dConfig
  dSecretTokenIOState <- GSecretToken.initIOState
  let dDatabaseConnectionConfig = Cf.cfDatabaseConfig dConfig
      dAppURIConfig = Cf.cfAppURIConfig dConfig
  pure
    ( loggerWorker
    , Deps
        { dConfig
        , dLoggerHandle
        , dDatabaseConnectionConfig
        , dPageSpecParserHandle =
            Core.Pagination.Impl.new $ Cf.cfMaxPageLimit dConfig
        , dLoadJSONRequestBody =
            liftIO .
            RequestBodyLoader.loadJSONRequestBody
              RequestBodyLoader.Config
                {cfMaxBodySize = Cf.cfMaxRequestJsonBodySize dConfig}
        , dSecretTokenIOState
        , dAppURIConfig
        , dRepresentationBuilderHandle =
            RepBuilderHandle
              { hJSONEncode =
                  JSONEncoder.encode
                    JSONEncoder.Config
                      {prettyPrint = Cf.cfJSONPrettyPrint dConfig}
              , hRenderAppURI = Web.AppURI.renderAppURI dAppURIConfig
              }
        })

getWebEntryPointHandle :: Deps -> IO Web.EntryPoint.Handle
getWebEntryPointHandle deps@Deps {..} = do
  hState <- Web.EntryPoint.makeState
  pure
    Web.EntryPoint.Handle
      { hState
      , hLogger = (`sessionLoggerHandle` dLoggerHandle)
      , hHandlers = injectDependenciesToHandler deps <$> Handlers.handlers
      , hShowInternalExceptionInfoInResponses =
          Cf.cfShowInternalErrorInfoInResponse dConfig
      , hPresentCoreException =
          presentCoreException dRepresentationBuilderHandle
      , hPresentWebException = presentWebException dRepresentationBuilderHandle
      , hNotFoundResponse = notFoundResponse
      , hMethodNotAllowedResponse = methodNotAllowedResponse
      , hUncaughtExceptionResponseForDebug = uncaughtExceptionResponseForDebug
      }

injectDependenciesToHandler ::
     Deps -> Handlers.Handler -> Web.ApplicationWithSession
injectDependenciesToHandler deps handler session =
  transactionApplicationToIOApplication (databaseHandleWith deps session) $
  handler (handlersDepsWith deps session)

transactionApplicationToIOApplication ::
     Database.Handle
  -> Web.GenericApplication Database.Transaction
  -> Web.GenericApplication IO
transactionApplicationToIOApplication h =
  Web.mapGenericApplication (Database.runTransactionRW h) liftIO

handlersDepsWith :: Deps -> Web.Session -> Handlers.Deps
handlersDepsWith deps@Deps {..} session =
  Handlers.Deps
    { dConfig
    , dPageSpecParserHandle
    , dLoadJSONRequestBody
    , dSecretTokenIOState
    , dAppURIConfig
    , dRepresentationBuilderHandle
    , dAuthenticate =
        Core.Authentication.authenticate $ authenticationHandleWith deps session
    }

databaseHandleWith :: Deps -> Web.Session -> Database.Handle
databaseHandleWith Deps {..} session =
  Database.Handle
    { hConnectionConfig = dDatabaseConnectionConfig
    , hLoggerHandle = sessionLoggerHandle session dLoggerHandle
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

-- | Creates an IO action and a logger handle. The IO action must be
-- forked in order for logging to work.
getLoggerHandle :: Cf.Config -> IO (Logger.Impl.Worker, Logger.Handle IO)
getLoggerHandle Cf.Config {..} = do
  hFileHandle <- getFileHandle cfLogFile
  Logger.Impl.new
    Logger.Impl.Handle {hFileHandle, hMinLevel = cfLoggerVerbosity}
  where
    getFileHandle (Cf.LogFilePath path) =
      openFile path AppendMode `catchS` \e ->
        die $ "While opening log file: " ++ displayException (e :: IOException)
    getFileHandle Cf.LogFileStdErr = pure stderr

sessionLoggerHandle :: Web.Session -> Logger.Handle IO -> Logger.Handle IO
sessionLoggerHandle Web.Session {..} =
  Logger.mapMessage $ \text -> "SID-" <> showAsText sessionId <> " " <> text
