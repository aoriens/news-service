{-# LANGUAGE RecordWildCards #-}

-- | High-level interface to dynamic configuration data (configuration
-- files, command line parameters etc). It should only allow getting
-- and parsing settings to simple data types as they are, neither
-- providing default values nor parsing to library-specific types.
module Config
  ( getConfig
  , Config(..)
  ) where

import Control.Exception.Sync
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Core.ExactConversion
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DC
import qualified Data.HashMap.Lazy as LHM
import Data.Int
import Data.Ratio
import Data.Word
import System.Environment
import System.Exit
import Text.Printf

-- | Whole configuration data.
data Config =
  Config
    { cfServerPort :: Maybe Int
    , cfServerHostPreference :: Maybe String
    , cfServerName :: Maybe String
    , cfDatabaseName :: String
    , cfDatabaseHost :: Maybe String
    , cfDatabasePort :: Maybe Word16
    , cfDatabaseUser :: Maybe String
    , cfDatabasePassword :: Maybe String
    , cfLoggerVerbosity :: Maybe String
    , cfLogFilePath :: Maybe String
    , cfCoreMaxPageLimit :: Maybe Int32
    , cfCoreMaxRequestJsonBodySize :: Maybe Word64
    , cfDebugShowInternalErrorInfoInResponse :: Maybe Bool
    , cfDebugJSONPrettyPrint :: Maybe Bool
    }
  deriving (Show)

-- | Loads and parses configuration data from all sources
-- (configuration files etc).
getConfig :: IO Config
getConfig = do
  conf <- loadConfigFile
  runReaderT parseConfig conf `catchS` \(DC.KeyError key) ->
    die $ printf "Configuration option '%v' is missing" key

parseConfig :: ReaderT DC.Config IO Config
parseConfig = do
  cfServerPort <- lookupOpt "server.port"
  cfServerHostPreference <- lookupOpt "server.host"
  cfServerName <- lookupOpt "server.name"
  cfDatabaseName <- require "postgresql.database_name"
  cfDatabaseHost <- lookupOpt "postgresql.host"
  cfDatabasePort <- fmap getExactIntegral <$> lookupOpt "postgresql.port"
  cfDatabaseUser <- lookupOpt "postgresql.user"
  cfDatabasePassword <- lookupOpt "postgresql.password"
  cfLoggerVerbosity <- lookupOpt "log.verbosity"
  cfLogFilePath <- lookupOpt "log.path"
  cfCoreMaxPageLimit <-
    fmap getExactIntegral <$> lookupOpt "core.max_page_limit"
  cfCoreMaxRequestJsonBodySize <-
    fmap getExactIntegral <$> lookupOpt "core.max_request_json_body_size"
  cfDebugShowInternalErrorInfoInResponse <-
    lookupOpt "debug.show_internal_errors"
  cfDebugJSONPrettyPrint <- lookupOpt "debug.json_pretty_print"
  pure Config {..}

loadConfigFile :: IO DC.Config
loadConfigFile = do
  paths <- getConfigPaths
  DC.load $ map DC.Required paths

getConfigPaths :: IO [FilePath]
getConfigPaths = do
  args <- getArgs
  case args of
    ("--config":path:_) -> pure [path]
    _ -> die "Option '--config PATH_TO_CONFIG_FILE' is required"

lookupOpt :: (DC.Configured a) => DC.Name -> ReaderT DC.Config IO (Maybe a)
lookupOpt key = do
  hmap <- lift . DC.getMap =<< ask
  case LHM.lookup key hmap of
    Nothing -> pure Nothing
    Just rawValue ->
      case DC.convert rawValue of
        Nothing -> lift $ rejectValue rawValue
        Just v -> pure $ Just v
  where
    rejectValue value =
      die $
      printf
        "Configuration option '%v' is assigned an incorrect value: '%v'"
        key
        (show value)

require :: (DC.Configured a) => DC.Name -> ReaderT DC.Config IO a
require key = do
  conf <- ask
  lift $ DC.require conf key

-- | An integral type wrapper with a stricter decoding policy. It
-- tries to decode a value without loss of precision. Normally, trying
-- to represent 9999999 as 'Word16' results in something like 16959,
-- which is unsafe when dealing with configuration files.
newtype ExactIntegral a =
  ExactIntegral
    { getExactIntegral :: a
    }

instance Integral a => DC.Configured (ExactIntegral a) where
  convert (DC.Number n)
    | denominator n == 1 = ExactIntegral <$> fromIntegralExact (numerator n)
    | otherwise = Nothing
  convert _ = Nothing
