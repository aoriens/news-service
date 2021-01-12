{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Service.Transaction
  ( Transaction
  , runTransactionRW
  , runStatement
  , Handle(..)
  ) where

import qualified Control.Exception as IOE
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Service.ConnectionManager as ConnectionManager
import {-# SOURCE #-} Database.Service.Exception
import qualified Hasql.Connection as C
import qualified Hasql.Decoders as D
import qualified Hasql.Session as S
import qualified Hasql.Statement as St
import qualified Logger
import Prelude hiding (log)

data Handle =
  Handle
    { hConnectionConfig :: ConnectionManager.Config
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
  ConnectionManager.withConnection hConnectionConfig $ \envConnection ->
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

-- | Creates a session of a single, atomic transaction. Thus, several
-- transactions can be composed into a single session.
transactionRW :: Transaction a -> Session a
transactionRW (Transaction transactionSession) =
  (`onException` rollback) $ do
    startTransaction
    r <- transactionSession
    commit
    pure r

startTransaction :: Session ()
startTransaction =
  simplePreparedStatement "start transaction isolation level serializable"

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
