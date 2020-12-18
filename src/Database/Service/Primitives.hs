{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Database-related basic definitions. They may only be imported
-- from database-related modules.
module Database.Service.Primitives
  ( Handle(..)
  , Session
  , runSession
  , Transaction
  , St.Statement(..)
  , runStatement
  , transactionRO
  , transactionRW
  , runTransactionRO
  , runTransactionRW
  , DatabaseException(..)
  , S.QueryError
  , databaseInternalInconsistency
  , databaseUnsafeFromJust
  ) where

import qualified Control.Exception as IOE
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Service.ConnectionManager as CM
import qualified Hasql.Connection as C
import qualified Hasql.Decoders as D
import qualified Hasql.Session as S
import qualified Hasql.Statement as St
import qualified Logger
import Prelude hiding (log)

data Handle =
  Handle
    { hConnectionConfig :: CM.Config
    , hLoggerHandle :: Logger.Handle IO
    }

-- | The SQL session - a monad containing SQL statements,
--  transactions, and IO actions. It supersedes 'S.Session' to perform
--  additional processing when producing sessions from statements.
newtype Session a =
  Session (ReaderT SessionEnv S.Session a)
  deriving (Functor, Applicative, Monad, MonadIO)

data SessionEnv =
  SessionEnv
    { envLoggerHandle :: Logger.Handle IO
    , envConnection :: C.Connection
    }

instance MonadThrow Session where
  throwM = liftIO . IOE.throwIO

instance MonadCatch Session where
  catch action handler =
    Session $ do
      env <- ask
      liftIO $
        catch (runSessionWithEnv env action) (runSessionWithEnv env . handler)

-- | Runs a session. It can throw 'DatabaseException'.
runSession :: Handle -> Session a -> IO a
runSession Handle {..} session =
  CM.withConnection hConnectionConfig $ \envConnection ->
    let env = SessionEnv {envConnection, envLoggerHandle = hLoggerHandle}
     in runSessionWithEnv env session

runSessionWithEnv :: SessionEnv -> Session a -> IO a
runSessionWithEnv env (Session session) =
  either (IOE.throwIO . HasqlException) pure =<<
  S.run hasqlSession (envConnection env)
  where
    hasqlSession = runReaderT session env

type SQL = B.ByteString

-- | The SQL transaction - a composable group of SQL statements that
-- are followed with an implicit transaction commit or rollback
-- statement.
newtype Transaction a =
  Transaction (Session a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

-- | Creates a composable transaction from a statement.
runStatement :: St.Statement a b -> a -> Transaction b
runStatement st = Transaction . freeStatement st

freeStatement :: St.Statement a b -> a -> Session b
freeStatement st@(St.Statement sql _ _ _) params = do
  log Logger.Debug $ "Run SQL: " <> T.decodeLatin1 sql
  Session . lift $ S.statement params st

log :: Logger.Level -> T.Text -> Session ()
log level text =
  Session $ do
    loggerH <- asks envLoggerHandle
    liftIO $ Logger.log loggerH level text

transactionRW :: Transaction a -> Session a
transactionRW = transactionWithMode ReadWrite

transactionRO :: Transaction a -> Session a
transactionRO = transactionWithMode ReadOnly

data ReadWriteMode
  = ReadOnly
  | ReadWrite

-- | Creates a session of a single, atomic transaction. Thus, several
-- transactions can be composed into a single session.
transactionWithMode :: ReadWriteMode -> Transaction a -> Session a
transactionWithMode rwMode (Transaction transactionSession) =
  (`onException` rollback) $ do
    startTransaction rwMode
    r <- transactionSession
    commit
    pure r

startTransaction :: ReadWriteMode -> Session ()
startTransaction rwMode = simplePreparedStatement sql
  where
    sql =
      case rwMode of
        ReadWrite -> "start transaction isolation level serializable read write"
        ReadOnly -> "start transaction isolation level serializable read only"

commit :: Session ()
commit = simplePreparedStatement "commit"

rollback :: Session ()
rollback = simplePreparedStatement "rollback"

simplePreparedStatement :: SQL -> Session ()
simplePreparedStatement sql =
  St.Statement sql mempty D.noResult True `freeStatement` ()

-- | Creates and runs a session from a single read-write transaction.
-- It can throw 'DatabaseException'.
runTransactionRW :: Handle -> Transaction a -> IO a
runTransactionRW h = runSession h . transactionRW

-- | Creates and runs a session from a single read-only transaction.
-- It can throw 'DatabaseException'.
runTransactionRO :: Handle -> Transaction a -> IO a
runTransactionRO h = runSession h . transactionRO

data DatabaseException
  = HasqlException S.QueryError
  | DatabaseInternalInconsistencyException T.Text
  deriving (Show)

instance Exception DatabaseException

databaseInternalInconsistency :: T.Text -> Transaction a
databaseInternalInconsistency = throwM . DatabaseInternalInconsistencyException

-- | This is similar to 'fromJust', but throws
-- DatabaseInternalInconsistencyException if the argument is Nothing.
-- Additionally, it requires a textual description for the error case
-- in order to log it. This is useful sometimes when there are some
-- invariants in the database that the business logic can rely on,
-- e.g. creating a news from a news draft must always succeed if the
-- draft is determined to exist before.
databaseUnsafeFromJust :: T.Text -> Maybe a -> Transaction a
databaseUnsafeFromJust errorDescription =
  maybe (databaseInternalInconsistency errorDescription) pure
