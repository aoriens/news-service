{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  ) where

import qualified Config as Cf
import Control.Concurrent.Async
import Control.Exception
import Control.Exception.Sync
import qualified Core.Interactor.GetNews
import Data.String
import qualified Data.Text as T
import qualified Database
import qualified Database.ConnectionManager as DBConnManager
import qualified Gateway.News
import qualified Logger
import qualified Logger.Impl
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Handler.Warp as Warp
import System.Exit
import System.IO hiding (Handle)
import qualified Web.Application
import qualified Web.Handler.News as HNews
import qualified Web.Router as R
import qualified Web.Types as Web

-- Some common module dependencies. Its purpose is to be passed to
-- functions **in this module**, keeping extensibility in the number
-- of fields and avoiding to add excess parameters to 100500 function
-- signatures.
data Deps =
  Deps
    { dWithDBConnection :: forall a. Database.WithConnection a
    , dConfig :: Cf.Config
    , dLoggerHandle :: Logger.Handle IO
    }

main :: IO ()
main = do
  (loggerWorker, deps@Deps {..}) <- getDeps
  webHandle <- getWebAppHandle deps
  race_ loggerWorker $ do
    Logger.info dLoggerHandle "Starting Warp"
    Warp.runSettings
      (warpSettings dConfig)
      (Web.Application.application webHandle)

warpSettings :: Cf.Config -> Warp.Settings
warpSettings Cf.Config {..} =
  maybe id (Warp.setServerName . fromString) cfServerName .
  maybe id (Warp.setHost . fromString) cfServerHostPreference .
  maybe id Warp.setPort cfServerPort $
  Warp.setHost "localhost" Warp.defaultSettings

getDeps :: IO (Logger.Impl.Worker, Deps)
getDeps = do
  dConfig <- Cf.getConfig
  (loggerWorker, dLoggerHandle) <- getLoggerHandle dConfig
  let dWithDBConnection =
        DBConnManager.withConnection (makeDBConnectionConfig dConfig)
  pure (loggerWorker, Deps {..})

makeDBConnectionConfig :: Cf.Config -> DBConnManager.Config
makeDBConnectionConfig Cf.Config {..} =
  DBConnManager.makeConfig $
  (DBConnManager.connectionSettingsWithDatabaseName $ fromString cfDatabaseName)
    { DBConnManager.settingsHost = fromString <$> cfDatabaseHost
    , DBConnManager.settingsPort = cfDatabasePort
    , DBConnManager.settingsUser = fromString <$> cfDatabaseUser
    , DBConnManager.settingsPassword = fromString <$> cfDatabasePassword
    }

getWebAppHandle :: Deps -> IO Web.Application.Handle
getWebAppHandle deps@Deps {..} = do
  let hLogger = (`sessionLoggerHandle` dLoggerHandle)
  hState <- Web.Application.makeState
  let hRouter = router deps
  pure Web.Application.Handle {..}

router :: Deps -> R.Router
router deps =
  R.new $ do
    R.ifPath ["news"] $ do
      R.ifMethod Http.methodGet $ HNews.run . newsHandlerHandle deps

newsHandlerHandle :: Deps -> Web.Session -> HNews.Handle
newsHandlerHandle Deps {..} session =
  HNews.Handle
    (Core.Interactor.GetNews.Handle
       (Gateway.News.getNews
          Gateway.News.Handle
            { Gateway.News.hWithConnection = dWithDBConnection
            , Gateway.News.hLoggerHandle =
                sessionLoggerHandle session dLoggerHandle
            }))

-- | Creates an IO action and a logger handle. The IO action must be
-- forked in order for logging to work.
getLoggerHandle :: Cf.Config -> IO (Logger.Impl.Worker, Logger.Handle IO)
getLoggerHandle Cf.Config {..} = do
  hFileHandle <- getFileHandle cfLogFilePath
  hMinLevel <- either die pure (parseVerbosity cfLoggerVerbosity)
  Logger.Impl.new Logger.Impl.Handle {..}
  where
    getFileHandle (Just path@(_:_)) =
      openFile path AppendMode `catchS` \e ->
        die $ "While opening log file: " ++ displayException (e :: IOException)
    getFileHandle _ = pure stderr
    parseVerbosity v
      | Nothing <- v = Right Logger.Info
      | v == Just "debug" = Right Logger.Debug
      | v == Just "info" = Right Logger.Info
      | v == Just "warning" = Right Logger.Warning
      | v == Just "error" = Right Logger.Error
      | Just s <- v = Left $ "Logger verbosity is set incorrectly: " ++ show s

sessionLoggerHandle :: Web.Session -> Logger.Handle IO -> Logger.Handle IO
sessionLoggerHandle Web.Session {..} =
  Logger.mapMessage $ \text -> "SID-" <> T.pack (show sessionId) <> " " <> text
