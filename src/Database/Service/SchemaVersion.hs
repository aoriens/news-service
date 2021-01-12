{-# LANGUAGE QuasiQuotes #-}

module Database.Service.SchemaVersion
  ( check
  , Failure(..)
  , MigrationsDirectoryPath
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Char
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Profunctor
import qualified Data.Text as T
import qualified Database.Service.Exception as DBException
import Database.Service.Transaction
import qualified Hasql.TH as TH
import System.Directory
import System.FilePath
import Text.Read (readMaybe)

-- | Checks if the current database schema version is appropriate.
check :: Handle -> MigrationsDirectoryPath -> IO (Either Failure ())
check h migrationsPath =
  runExceptT $ do
    schemaVersion <- getSchemaVersion h
    latestMigrationSchemaVersion <-
      getLatestMigrationSchemaVersion migrationsPath
    if schemaVersion == latestMigrationSchemaVersion
      then pure ()
      else throwE $ MigrationRequiredToVersion latestMigrationSchemaVersion

type MigrationsDirectoryPath = FilePath

data Failure
  = MigrationRequiredToVersion Int
  | CannotParseSchemaVersion
  | DatabaseException DBException.DatabaseException
  | MigrationDirectoryAccessException IOException
  | NoMigrationsFound
  deriving (Show)

getSchemaVersion :: Handle -> ExceptT Failure IO Int
getSchemaVersion h =
  lift (try $ runTransactionRW h getSchemaVersionTransaction) >>= \case
    Left dbException -> throwE $ DatabaseException dbException
    Right Nothing -> throwE CannotParseSchemaVersion
    Right (Just version) -> pure version

getSchemaVersionTransaction :: Transaction (Maybe Int)
getSchemaVersionTransaction =
  (`runStatement` ()) $
  rmap
    (readMaybe . T.unpack)
    [TH.singletonStatement|
      select value :: varchar
      from config
      where key = 'schema_version'
    |]

getLatestMigrationSchemaVersion ::
     MigrationsDirectoryPath -> ExceptT Failure IO Int
getLatestMigrationSchemaVersion migrationsPath =
  lift (try $ maximumVersionFromFileNames <$> listFiles) >>= \case
    Left ioException -> throwE $ MigrationDirectoryAccessException ioException
    Right Nothing -> throwE NoMigrationsFound
    Right (Just maxVersion) -> pure maxVersion
  where
    listFiles =
      getDirectoryContents migrationsPath >>=
      filterM (doesFileExist . (migrationsPath </>))
    maximumVersionFromFileNames =
      fmap maximum .
      NonEmpty.nonEmpty . mapMaybe schemaVersionFromMigrationFileName

schemaVersionFromMigrationFileName :: FilePath -> Maybe Int
schemaVersionFromMigrationFileName name
  | suffix == ".sql" && all isDigit prefix = readMaybe prefix
  | otherwise = Nothing
  where
    (prefix, suffix) = break (== '.') name
