{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Database-related basic definitions. They may only be imported
-- from gateways.
module Database
  ( Session
  , WithConnection
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
import qualified Hasql.Connection as C
import qualified Hasql.Session as S
import qualified Hasql.Statement as St
import qualified Logger

-- | An SQL session - a monad containing SQL statements and optional
-- IO actions. It supersedes 'S.Session' to perform additional
-- processing when producing sessions from statements.
newtype Session a =
  Session (ReaderT (Logger.Handle IO) S.Session a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | A type of a computation provided with a database connection. See
-- 'Database.ConnectionManager'.
type WithConnection a = (C.Connection -> IO a) -> IO a

-- | Runs a session. It can throw 'QueryException'.
runSession :: WithConnection a -> Logger.Handle IO -> Session a -> IO a
runSession withConnection loggerH (Session session) =
  withConnection $ S.run hasqlSession >=> either (throwIO . QueryException) pure
  where
    hasqlSession = runReaderT session loggerH

-- | Creates a session from a statement.
statement :: St.Statement params result -> params -> Session result
statement st@(St.Statement sql _ _ _) params =
  Session $ do
    loggerH <- ask
    liftIO $ Logger.debug loggerH ("Run SQL: " <> T.decodeLatin1 sql)
    lift $ S.statement params st

-- | A shortcut to run a single statement as a session. It can throw
-- 'QueryException'.
runStatement ::
     WithConnection out
  -> Logger.Handle IO
  -> St.Statement params out
  -> params
  -> IO out
runStatement withConnection loggerH st =
  runSession withConnection loggerH . statement st

newtype QueryException =
  QueryException S.QueryError
  deriving (Show)

instance Exception QueryException
