{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | Database-related basic definitions. They may only be imported
-- from gateways.
module Database
  ( Handle(..)
  , Session
  , runSession
  , statement
  , runStatement
  , QueryException(..)
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text.Encoding as T
import qualified Database.ConnectionManager as CM
import qualified Hasql.Session as S
import qualified Hasql.Statement as St
import qualified Logger

data Handle =
  Handle
    { hConnectionConfig :: CM.Config
    , hLoggerHandle :: Logger.Handle IO
    }

-- | An SQL session - a monad containing SQL statements and optional
-- IO actions. It supersedes 'S.Session' to perform additional
-- processing when producing sessions from statements.
newtype Session a =
  Session (ReaderT (Logger.Handle IO) S.Session a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Runs a session. It can throw 'QueryException'.
runSession :: Handle -> Session a -> IO a
runSession Handle {..} (Session session) =
  CM.withConnection hConnectionConfig $
  S.run hasqlSession >=> either (throwIO . QueryException) pure
  where
    hasqlSession = runReaderT session hLoggerHandle

-- | Creates a session from a statement.
statement :: St.Statement params result -> params -> Session result
statement st@(St.Statement sql _ _ _) params =
  Session $ do
    loggerH <- ask
    liftIO $ Logger.debug loggerH ("Run SQL: " <> T.decodeLatin1 sql)
    lift $ S.statement params st

-- | A shortcut to run a single statement as a session. It can throw
-- 'QueryException'.
runStatement :: Handle -> St.Statement params out -> params -> IO out
runStatement h st = runSession h . statement st

newtype QueryException =
  QueryException S.QueryError
  deriving (Show)

instance Exception QueryException
