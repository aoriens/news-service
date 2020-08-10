-- | Database-related basic definitions. They may only be imported
-- from gateways.
module Database
  ( runSession
  , runStatement
  , QueryException(..)
  , WithConnection
  ) where

import Control.Exception
import Control.Monad
import qualified Hasql.Connection as C
import qualified Hasql.Session as S
import qualified Hasql.Statement as St

-- | A type of a computation provided with a database connection. See
-- 'Database.ConnectionManager'.
type WithConnection a = (C.Connection -> IO a) -> IO a

-- | Runs a Hasql session. It can throw 'QueryException'.
runSession :: WithConnection a -> S.Session a -> IO a
runSession withConnection session =
  withConnection $ S.run session >=> either (throwIO . QueryException) pure

-- | Runs a single Hasql statement as a session. It can throw
-- 'QueryException'.
runStatement ::
     WithConnection out -> params -> St.Statement params out -> IO out
runStatement withConnection params =
  runSession withConnection . S.statement params

newtype QueryException =
  QueryException S.QueryError
  deriving (Show)

instance Exception QueryException
