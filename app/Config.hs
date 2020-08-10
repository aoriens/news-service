{-# LANGUAGE RecordWildCards #-}

-- | High-level interface to dynamic configuration data (configuration
-- files, command line parameters etc). It should only allow getting
-- and parsing settings as-is to simple data types, neither providing
-- default values nor parsing to library-specific types.
module Config
  ( getConfig
  , Config(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import System.Environment

-- | Whole configuration data.
data Config =
  Config
    { cfServerPort :: Maybe Int
    , cfServerHostPreference :: Maybe String
    , cfServerName :: Maybe String
    }
  deriving (Show)

-- | Loads and parses configuration data from all sources
-- (configuration files etc).
getConfig :: IO Config
getConfig = do
  conf <- loadConfigFile
  (`runReaderT` conf) $ do
    cfServerPort <- lookupOpt "server.port"
    cfServerHostPreference <- lookupOpt "server.host"
    cfServerName <- lookupOpt "server.name"
    pure Config {..}

loadConfigFile :: IO C.Config
loadConfigFile = do
  paths <- getConfigPaths
  C.load $ map C.Required paths

getConfigPaths :: IO [FilePath]
getConfigPaths = do
  args <- getArgs
  case args of
    ("--config":path:_) -> pure [path]
    _ -> pure []

lookupOpt :: (C.Configured a) => C.Name -> ReaderT C.Config IO (Maybe a)
lookupOpt key = do
  conf <- ask
  lift $ C.lookup conf key
