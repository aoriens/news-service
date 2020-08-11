{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  ) where

import qualified Config as Cf
import Control.Exception
import Control.Exception.Sync
import Data.String
import qualified Database.ConnectionManager as DBConnManager
import qualified Gateway.News
import qualified Hasql.Connection
import qualified Interactor.GetNews
import qualified Logger
import qualified Logger.Impl
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Handler.Warp as Warp
import qualified Router as R
import System.Exit
import System.IO hiding (Handle)
import qualified Web
import qualified Web.Handler.News as HNews

-- Some common module dependencies. Its purpose is to be passed to
-- functions **in this module**, keeping extensibility in the number
-- of fields and avoiding to add excess parameters to 100500 function
-- signatures.
data Deps =
  Deps
    { dWithDBConnection :: forall a. (Hasql.Connection.Connection -> IO a) -> IO a
    , dConfig :: Cf.Config
    , dLoggerHandle :: Logger.Handle IO
    }

main :: IO ()
main = do
  deps@Deps {..} <- getDeps
  webHandle <- getWebHandle deps
  Logger.info dLoggerHandle "Starting Warp"
  Warp.runSettings (warpSettings dConfig) (Web.application webHandle)

warpSettings :: Cf.Config -> Warp.Settings
warpSettings Cf.Config {..} =
  maybe id (Warp.setServerName . fromString) cfServerName .
  maybe id (Warp.setHost . fromString) cfServerHostPreference .
  maybe id Warp.setPort cfServerPort $
  Warp.setHost "localhost" Warp.defaultSettings

getDeps :: IO Deps
getDeps = do
  dConfig <- Cf.getConfig
  dLoggerHandle <- getLoggerHandle dConfig
  let dWithDBConnection =
        DBConnManager.withConnection (makeDBConnectionConfig dConfig)
  pure Deps {..}

makeDBConnectionConfig :: Cf.Config -> DBConnManager.Config
makeDBConnectionConfig Cf.Config {..} =
  DBConnManager.makeConfig $
  (DBConnManager.connectionSettingsWithDatabaseName $ fromString cfDatabaseName)
    { DBConnManager.settingsHost = fromString <$> cfDatabaseHost
    , DBConnManager.settingsPort = cfDatabasePort
    , DBConnManager.settingsUser = fromString <$> cfDatabaseUser
    , DBConnManager.settingsPassword = fromString <$> cfDatabasePassword
    }

getWebHandle :: Deps -> IO Web.Handle
getWebHandle deps = do
  hLoggerHandle <- getLoggerHandle (dConfig deps)
  let hRouter = router deps
  pure Web.Handle {..}

router :: Deps -> R.Router
router deps =
  R.new $ do
    R.ifPath ["news"] $ do
      R.ifMethod Http.methodGet $ HNews.run (newsHandlerHandle deps)

newsHandlerHandle :: Deps -> HNews.Handle
newsHandlerHandle Deps {..} =
  HNews.Handle
    (Interactor.GetNews.Handle
       (Gateway.News.getNews
          Gateway.News.Handle {Gateway.News.hWithConnection = dWithDBConnection}))

getLoggerHandle :: Cf.Config -> IO (Logger.Handle IO)
getLoggerHandle Cf.Config {..} = do
  hFileHandle <- getFileHandle cfLogFilePath
  hMinLevel <- either die pure (parseVerbosity cfLoggerVerbosity)
  pure $ Logger.Impl.new Logger.Impl.Handle {..}
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
