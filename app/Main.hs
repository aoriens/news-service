{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import qualified Config as Cf
import Control.Exception
import Control.Exception.Sync
import Data.String
import qualified Database.ConnectionManager as DBConnManager
import qualified Gateway.News
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

-- The local environment containing configuration loaded from IO and
-- maybe some dependencies. It's purpose to be passed to pure
-- functions **in this module**, keeping extensibility in the number
-- of fields and avoiding to add excess parameters to 100500 function
-- signatures.
data Env =
  Env
    { eDBConnectionConfig :: DBConnManager.Config
    , eConfig :: Cf.Config
    , eLoggerHandle :: Logger.Handle IO
    }

main :: IO ()
main = do
  env@Env {..} <- getEnv
  webHandle <- getWebHandle env
  Logger.info eLoggerHandle "Starting Warp"
  Warp.runSettings (warpSettings eConfig) (Web.application webHandle)

warpSettings :: Cf.Config -> Warp.Settings
warpSettings Cf.Config {..} =
  maybe id (Warp.setServerName . fromString) cfServerName .
  maybe id (Warp.setHost . fromString) cfServerHostPreference .
  maybe id Warp.setPort cfServerPort $
  Warp.setHost "localhost" Warp.defaultSettings

getEnv :: IO Env
getEnv = do
  eConfig <- Cf.getConfig
  eLoggerHandle <- getLoggerHandle eConfig
  let eDBConnectionConfig = makeDBConnectionConfig eConfig
  pure Env {..}

makeDBConnectionConfig :: Cf.Config -> DBConnManager.Config
makeDBConnectionConfig Cf.Config {..} =
  DBConnManager.makeConfig $
  (DBConnManager.connectionSettingsWithDatabaseName $ fromString cfDatabaseName)
    { DBConnManager.settingsHost = fromString <$> cfDatabaseHost
    , DBConnManager.settingsPort = cfDatabasePort
    , DBConnManager.settingsUser = fromString <$> cfDatabaseUser
    , DBConnManager.settingsPassword = fromString <$> cfDatabasePassword
    }

getWebHandle :: Env -> IO Web.Handle
getWebHandle env = do
  hLoggerHandle <- getLoggerHandle (eConfig env)
  let hRouter = router env
  pure Web.Handle {..}

router :: Env -> R.Router
router env =
  R.new $ do
    R.ifPath ["news"] $ do
      R.ifMethod Http.methodGet $ HNews.run (newsHandlerHandle env)

newsHandlerHandle :: Env -> HNews.Handle
newsHandlerHandle Env {..} =
  HNews.Handle
    (Interactor.GetNews.Handle
       (Gateway.News.getNews
          Gateway.News.Handle
            { Gateway.News.hWithConnection =
                DBConnManager.withConnection eDBConnectionConfig
            }))

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
