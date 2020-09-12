{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Database-related basic definitions. They may only be imported
-- from database-related modules.
module Database
  ( Handle(..)
  , Session
  , runSession
  , Transaction
  , HSt.Statement(..)
  , HT.IsolationLevel(..)
  , HT.Mode(..)
  , statement
  , runTransaction
  , runTransactionRW
  , DatabaseException(..)
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as B
import qualified Data.DList as DL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.ConnectionManager as CM
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HSt
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HT
import qualified Logger

data Handle =
  Handle
    { hConnectionConfig :: CM.Config
    , hLoggerHandle :: Logger.Handle IO
    }

-- | The SQL session - a monad containing SQL statements and optional
-- IO actions. It supersedes 'HS.Session' to perform additional
-- processing when producing sessions from statements.
newtype Session a =
  Session (ReaderT (Logger.Handle IO) HS.Session a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Runs a session. It can throw 'DatabaseException'.
runSession :: Handle -> Session a -> IO a
runSession Handle {..} (Session session) =
  CM.withConnection hConnectionConfig $
  HS.run hasqlSession >=> either (throwIO . DatabaseException) pure
  where
    hasqlSession = runReaderT session hLoggerHandle

type SQL = B.ByteString

-- | The SQL transaction - a composable group of SQL statements that
-- are followed with an implicit transaction commit statement. In case
-- of a transaction conflict, it will restart automatically until
-- success.
newtype Transaction a =
  Transaction (StateT (DL.DList SQL) HT.Transaction a)
  deriving (Functor, Applicative, Monad)

-- | Creates a composable transaction from a statement.
statement :: HSt.Statement a b -> a -> Transaction b
statement st@(HSt.Statement sql _ _ _) params =
  Transaction $ do
    modify' (`DL.snoc` sql)
    lift $ HT.statement params st

transactionRW :: Transaction a -> Session a
transactionRW = transactionWithMode HT.Serializable HT.Write

transaction :: Transaction a -> Session a
transaction = transactionWithMode HT.Serializable HT.Read

-- | Creates a session of a single, atomic transaction. Thus, several
-- transactions can be composed into a single session.
transactionWithMode ::
     HT.IsolationLevel -> HT.Mode -> Transaction a -> Session a
transactionWithMode level mode (Transaction t) =
  Session $ do
    (r, sqls) <- lift $ HT.transaction level mode (runStateT t DL.empty)
    loggerH <- ask
    liftIO $ Logger.debug loggerH (logSQLs sqls)
    pure r
  where
    logSQLs sqls =
      "Executed SQL transaction:\n" <>
      T.intercalate "\n" (map ((<> ";") . T.decodeLatin1) $ DL.toList sqls)

-- | Creates and runs a session from a single read-write transaction.
-- It can throw 'DatabaseException'.
runTransactionRW :: Handle -> Transaction a -> IO a
runTransactionRW h = runSession h . transactionRW

-- | Creates and runs a session from a single read-only transaction.
-- It can throw 'DatabaseException'.
runTransaction :: Handle -> Transaction a -> IO a
runTransaction h = runSession h . transaction

newtype DatabaseException =
  DatabaseException HS.QueryError
  deriving (Show)

instance Exception DatabaseException
