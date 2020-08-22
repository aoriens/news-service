{-# LANGUAGE RecordWildCards #-}

-- | The module provides input for configuration values. It does not
-- contain high-level logic, such as default values or parsing to
-- application-specific high-level types. It should return them as-is.
module Config.IO
  ( getConfig
  ) where

import qualified Config as C
import Control.Exception.Sync
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Core.ExactConversion
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DC
import qualified Data.HashMap.Lazy as LHM
import Data.Ratio
import System.Environment
import System.Exit
import Text.Printf

-- | Loads and parses configuration data from all sources
-- (configuration files etc).
getConfig :: IO C.InConfig
getConfig = do
  conf <- loadConfigFile
  runReaderT parseConfig conf `catchS` \(DC.KeyError key) ->
    die $ printf "Configuration option '%v' is missing" key

parseConfig :: ReaderT DC.Config IO C.InConfig
parseConfig = do
  inServerPort <- lookupOpt "server.port"
  inServerHostPreference <- lookupOpt "server.host"
  inServerName <- lookupOpt "server.name"
  inDatabaseName <- require "postgresql.database_name"
  inDatabaseHost <- lookupOpt "postgresql.host"
  inDatabasePort <- fmap getExactIntegral <$> lookupOpt "postgresql.port"
  inDatabaseUser <- lookupOpt "postgresql.user"
  inDatabasePassword <- lookupOpt "postgresql.password"
  inLoggerVerbosity <- lookupOpt "log.verbosity"
  inLogFilePath <- lookupOpt "log.path"
  inCoreMaxPageLimit <-
    fmap getExactIntegral <$> lookupOpt "core.max_page_limit"
  inCoreMaxRequestJsonBodySize <-
    fmap getExactIntegral <$> lookupOpt "core.max_request_json_body_size"
  inDebugShowInternalErrorInfoInResponse <-
    lookupOpt "debug.show_internal_errors"
  inSecretTokenLength <-
    fmap getExactIntegral <$> lookupOpt "core.secret_token_length"
  inDebugJSONPrettyPrint <- lookupOpt "debug.json_pretty_print"
  pure C.InConfig {..}

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
