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
import qualified Data.HashSet as HS
import Data.Int
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Word
import qualified Database.ConnectionManager as DB
import qualified Logger
import qualified Network.Wai.Handler.Warp as Warp
import qualified Web.AppURL

-- | The high-level application configuration.
data Config =
  Config
    { cfWarpSettings :: !Warp.Settings
    , cfDatabaseConfig :: !DB.Config
    , cfLoggerVerbosity :: !Logger.Level
    , cfLogFile :: !LogFile
    , cfMaxPageLimit :: !PageLimit
    , cfMaxRequestJsonBodySize :: !Word64
    , cfSecretTokenLength :: !Int
    , cfAllowedImageMimeTypes :: HS.HashSet Text
    , cfAppURLConfig :: Web.AppURL.Config
    , cfShowInternalErrorInfoInResponse :: !Bool
    , cfJSONPrettyPrint :: !Bool
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
    , inServerPublicURLsUseHTTPS :: Maybe Bool
    , inServerPublicDomain :: String
    , inDatabaseName :: String
    , inDatabaseHost :: Maybe String
    , inDatabasePort :: Maybe Word16
    , inDatabaseUser :: Maybe String
    , inDatabasePassword :: Maybe String
    , inLoggerVerbosity :: Maybe String
    , inLogFilePath :: Maybe String
    , inMaxPageLimit :: Maybe Int32
    , inMaxRequestJsonBodySize :: Maybe Word64
    , inSecretTokenLength :: Maybe Int
    , inAllowedImageMimeTypes :: Maybe [Text]
    , inShowInternalErrorInfoInResponse :: Maybe Bool
    , inJSONPrettyPrint :: Maybe Bool
    }

-- | Creates configuration. Errors are reported as a 'Left' with a
-- string description.
makeConfig :: InConfig -> Either String Config
makeConfig inConfig@InConfig {..} = do
  cfLoggerVerbosity <- parseLoggerLevel inLoggerVerbosity
  cfSecretTokenLength <-
    assureRange 8 1024 "Secret token length parameter" $
    fromMaybe 32 inSecretTokenLength
  cfAllowedImageMimeTypes <-
    case inAllowedImageMimeTypes of
      Nothing -> pure $ HS.fromList ["image/jpeg", "image/png"]
      Just types@(_:_) -> pure $ HS.fromList types
      Just [] -> Left "Allowed image MIME types list must not be empty"
  Right
    Config
      { cfWarpSettings = warpSettings inConfig
      , cfDatabaseConfig = databaseConfig inConfig
      , cfLogFile =
          case inLogFilePath of
            Just path@(_:_) -> LogFilePath path
            _ -> LogFileStdErr
      , cfMaxPageLimit = PageLimit $ fromMaybe 100 inMaxPageLimit
      , cfMaxRequestJsonBodySize = fromMaybe 16384 inMaxRequestJsonBodySize
      , cfShowInternalErrorInfoInResponse =
          Just True == inShowInternalErrorInfoInResponse
      , cfJSONPrettyPrint = Just True == inJSONPrettyPrint
      , cfAppURLConfig =
          Web.AppURL.Config
            { cfUseHTTPS = Just True == inServerPublicURLsUseHTTPS
            , cfDomain = T.encodeUtf8 $ fromString inServerPublicDomain
            }
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

assureRange :: (Ord a, Show a) => a -> a -> String -> a -> Either String a
assureRange min_ max_ xName x
  | x >= min_ && x <= max_ = Right x
  | otherwise =
    Left $
    concat
      [ xName
      , " must fall into range "
      , show min_
      , "..."
      , show max_
      , ", but given value "
      , show x
      ]
