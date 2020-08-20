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
  , tstatement
  , transactionWithMode
  , transactionRW
  , runTransactionRW
  , QueryException(..)
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as BS
import qualified Data.DList as DL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.ConnectionManager as CM
import qualified Hasql.Session as S
import qualified Hasql.Statement as St
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HT
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

-- | Creates a session from a statement. It is executed in a separate,
-- auto-committed transaction.
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

type SQL = BS.ByteString

-- | An SQL transaction - a composable group of SQL statements that
-- are followed with an implicit transaction commit. In case of a
-- transaction conflict, it will restart automatically until success.
newtype Transaction a =
  Transaction (StateT (DL.DList SQL) HT.Transaction a)
  deriving (Functor, Applicative, Monad)

-- | Creates a composable transaction from a statement.
tstatement :: St.Statement a b -> a -> Transaction b
tstatement st@(St.Statement sql _ _ _) params =
  Transaction $ do
    modify' (`DL.snoc` sql)
    lift $ HT.statement params st

-- | Creates a session from a read-write transaction with the safest
-- isolation level. It is a recommended default.
transactionRW :: Transaction a -> Session a
transactionRW = transactionWithMode HT.Serializable HT.Write

-- | Creates a session from a transaction. Thus, several transactions
-- can be composed into a single session.
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
      T.intercalate "\n" (map T.decodeLatin1 $ DL.toList sqls)

runTransactionRW :: Handle -> Transaction a -> IO a
runTransactionRW h = runSession h . transactionRW

newtype QueryException =
  QueryException S.QueryError
  deriving (Show)

instance Exception QueryException
