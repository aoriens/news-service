{-# LANGUAGE RecordWildCards #-}

-- | The high level configuration module. It is responsible for
-- parsing the application-specific types from simpler data types and
-- providing default values, but not data input.
module Config
  ( makeConfig
  , Config(..)
  , LogFile(..)
  , InConfig(..)
  ) where

import Core.Pagination
import Data.Int
import Data.Maybe
import Data.String
import Data.Word
import qualified Database.ConnectionManager as DB
import qualified Logger
import qualified Network.Wai.Handler.Warp as Warp

-- | The high-level application configuration.
data Config =
  Config
    { cfWarpSettings :: !Warp.Settings
    , cfDatabaseConfig :: !DB.Config
    , cfLoggerVerbosity :: !Logger.Level
    , cfLogFile :: !LogFile
    , cfCoreMaxPageLimit :: !PageLimit
    , cfCoreMaxRequestJsonBodySize :: !Word64
    , cfDebugShowInternalErrorInfoInResponse :: !Bool
    , cfDebugJSONPrettyPrint :: !Bool
    }

data LogFile
  = LogFileStdErr
  | LogFilePath String

-- | Configuration data input.
data InConfig =
  InConfig
    { inServerPort :: Maybe Int
    , inServerHostPreference :: Maybe String
    , inServerName :: Maybe String
    , inDatabaseName :: String
    , inDatabaseHost :: Maybe String
    , inDatabasePort :: Maybe Word16
    , inDatabaseUser :: Maybe String
    , inDatabasePassword :: Maybe String
    , inLoggerVerbosity :: Maybe String
    , inLogFilePath :: Maybe String
    , inCoreMaxPageLimit :: Maybe Int32
    , inCoreMaxRequestJsonBodySize :: Maybe Word64
    , inDebugShowInternalErrorInfoInResponse :: Maybe Bool
    , inDebugJSONPrettyPrint :: Maybe Bool
    }

-- | Creates configuration. Errors are reported as a 'Left' with a
-- string description.
makeConfig :: InConfig -> Either String Config
makeConfig inConfig@InConfig {..} = do
  cfLoggerVerbosity <- parseLoggerLevel inLoggerVerbosity
  Right
    Config
      { cfWarpSettings = warpSettings inConfig
      , cfDatabaseConfig = databaseConfig inConfig
      , cfLogFile =
          case inLogFilePath of
            Just path@(_:_) -> LogFilePath path
            _ -> LogFileStdErr
      , cfCoreMaxPageLimit = PageLimit $ fromMaybe 100 inCoreMaxPageLimit
      , cfCoreMaxRequestJsonBodySize =
          fromMaybe 16384 inCoreMaxRequestJsonBodySize
      , cfDebugShowInternalErrorInfoInResponse =
          Just True == inDebugShowInternalErrorInfoInResponse
      , cfDebugJSONPrettyPrint = Just True == inDebugJSONPrettyPrint
      , ..
      }

warpSettings :: InConfig -> Warp.Settings
warpSettings InConfig {..} =
  maybe id (Warp.setServerName . fromString) inServerName .
  maybe id (Warp.setHost . fromString) inServerHostPreference .
  maybe id Warp.setPort inServerPort $
  Warp.setHost "localhost" Warp.defaultSettings

databaseConfig :: InConfig -> DB.Config
databaseConfig InConfig {..} =
  DB.makeConfig $
  (DB.connectionSettingsWithDatabaseName $ fromString inDatabaseName)
    { DB.settingsHost = fromString <$> inDatabaseHost
    , DB.settingsPort = inDatabasePort
    , DB.settingsUser = fromString <$> inDatabaseUser
    , DB.settingsPassword = fromString <$> inDatabasePassword
    }

parseLoggerLevel :: Maybe String -> Either String Logger.Level
parseLoggerLevel v
  | Nothing <- v = Right Logger.Info
  | v == Just "debug" = Right Logger.Debug
  | v == Just "info" = Right Logger.Info
  | v == Just "warning" = Right Logger.Warning
  | v == Just "error" = Right Logger.Error
  | Just s <- v = Left $ "Logger verbosity is set incorrectly: " ++ show s
