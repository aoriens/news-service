-- | The module is responsible for managing open database connections.
module Database.ConnectionManager
  ( withConnection
  , Config
  , makeConfig
  , ConnectionSettings(..)
  , connectionSettingsWithDatabaseName
  , ConnectionException
  ) where

import Control.Exception
import qualified Data.ByteString as SBS
import Data.Maybe
import Data.Word
import qualified Hasql.Connection as C

-- | Runs a given function with a connection. It can throw
-- 'ConnectionException'.
withConnection :: Config -> (C.Connection -> IO a) -> IO a
withConnection (Config settings) = bracket getConnection C.release
  where
    getConnection = do
      r <- C.acquire settings
      case r of
        Left e -> throwIO (ConnectionException e)
        Right conn -> pure conn

newtype Config =
  Config C.Settings

makeConfig :: ConnectionSettings -> Config
makeConfig ConnectionSettings {..} =
  Config $
  C.settings
    (fromMaybe "" settingsHost)
    (fromMaybe 0 settingsPort)
    (fromMaybe "" settingsUser)
    (fromMaybe "" settingsPassword)
    settingsDatabaseName

data ConnectionSettings =
  ConnectionSettings
    { settingsHost :: Maybe SBS.ByteString
    , settingsPort :: Maybe Word16
    , settingsDatabaseName :: SBS.ByteString
    , settingsUser :: Maybe SBS.ByteString
    , settingsPassword :: Maybe SBS.ByteString
    }

connectionSettingsWithDatabaseName :: SBS.ByteString -> ConnectionSettings
connectionSettingsWithDatabaseName databaseName =
  ConnectionSettings
    { settingsHost = Nothing
    , settingsPort = Nothing
    , settingsUser = Nothing
    , settingsPassword = Nothing
    , settingsDatabaseName = databaseName
    }

newtype ConnectionException =
  ConnectionException C.ConnectionError
  deriving (Show)

instance Exception ConnectionException
