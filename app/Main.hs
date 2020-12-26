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
import Core.Pagination
import qualified Core.Pagination.Impl
import qualified CreateHandlers
import qualified Data.Aeson as A
import Data.Text.Show
import qualified Database.Service.ConnectionManager as DBConnManager
import qualified Database.Service.Primitives as Database
import qualified FrontEnd.Wai
import qualified Gateway.SecretToken as GSecretToken
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
getWebEntryPointHandle Deps {..} = do
  hState <- Web.EntryPoint.makeState
  pure
    Web.EntryPoint.Handle
      { hState
      , hLogger = (`sessionLoggerHandle` dLoggerHandle)
      , hHandlers = handlers
      , hShowInternalExceptionInfoInResponses =
          Cf.cfShowInternalErrorInfoInResponse dConfig
      , hPresentCoreException =
          presentCoreException dRepresentationBuilderHandle
      , hPresentWebException = presentWebException dRepresentationBuilderHandle
      , hNotFoundResponse = notFoundResponse
      , hMethodNotAllowedResponse = methodNotAllowedResponse
      , hUncaughtExceptionResponseForDebug = uncaughtExceptionResponseForDebug
      }
  where
    handlers =
      CreateHandlers.createWith
        CreateHandlers.Handle
          { hDatabaseConnectionConfig = dDatabaseConnectionConfig
          , hConfig = dConfig
          , hLoggerHandleWith = (`sessionLoggerHandle` dLoggerHandle)
          , hPageSpecParserHandle = dPageSpecParserHandle
          , hLoadJSONRequestBody = dLoadJSONRequestBody
          , hSecretTokenIOState = dSecretTokenIOState
          , hAppURIConfig = dAppURIConfig
          , hRepresentationBuilderHandle = dRepresentationBuilderHandle
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
