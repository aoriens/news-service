{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Database-related basic definitions. They may only be imported
-- from database-related modules.
module Database
  ( Handle(..)
  , Session
  , runSession
  , Transaction
  , St.Statement(..)
  , HT.IsolationLevel(..)
  , HT.Mode(..)
  , statement
  , transaction
  , transactionRW
  , runTransaction
  , runTransactionRW
  , DatabaseException(..)
  , S.QueryError
  , isDatabaseResultErrorWithCode
  , onForeignKeyViolation
  , ignoringForeignKeyViolation
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as B
import qualified Data.DList as DL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.ConnectionManager as CM
import qualified Hasql.Session as S
import qualified Hasql.Statement as St
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HT
import qualified Logger
import qualified PostgreSQL.ErrorCodes as PE

data Handle =
  Handle
    { hConnectionConfig :: CM.Config
    , hLoggerHandle :: Logger.Handle IO
    }

-- | The SQL session - a monad containing SQL statements and optional
-- IO actions. It supersedes 'S.Session' to perform additional
-- processing when producing sessions from statements.
newtype Session a =
  Session (ReaderT (Logger.Handle IO) S.Session a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadError S.QueryError Session where
  throwError = Session . lift . throwError
  catchError (Session r) h =
    Session $ do
      env <- ask
      lift $
        catchError (runReaderT r env) $ \e ->
          let (Session r') = h e
           in runReaderT r' env

-- | Runs a session. It can throw 'DatabaseException'.
runSession :: Handle -> Session a -> IO a
runSession Handle {..} (Session session) =
  CM.withConnection hConnectionConfig $
  S.run hasqlSession >=> either (throwIO . DatabaseException) pure
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
statement :: St.Statement a b -> a -> Transaction b
statement st@(St.Statement sql _ _ _) params =
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
  DatabaseException S.QueryError
  deriving (Show)

instance Exception DatabaseException

isDatabaseResultErrorWithCode :: PE.ErrorCode -> S.QueryError -> Bool
isDatabaseResultErrorWithCode code queryError
  | (S.QueryError _ _ resultError) <- queryError
  , (S.ResultError (S.ServerError code' _ _ _)) <- resultError = code == code'
  | otherwise = False

-- | Runs a fallback session if another session finished with the
-- foreign key violation failure.
onForeignKeyViolation :: Session a -> Session a -> Session a
onForeignKeyViolation action fallback =
  catchError action $ \e ->
    if isDatabaseResultErrorWithCode PE.foreign_key_violation e
      then fallback
      else throwError e

ignoringForeignKeyViolation :: Session () -> Session ()
ignoringForeignKeyViolation action = action `onForeignKeyViolation` pure ()
