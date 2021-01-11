module Database.Service.Exception
  ( DatabaseException(..)
  , databaseInternalInconsistency
  , databaseUnsafeFromJust
  ) where

import Control.Exception
import Control.Monad.Catch
import Data.Maybe.Util
import qualified Data.Text as T
import Database.Service.Transaction
import qualified Hasql.Session as S

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
  fromMaybeM $ databaseInternalInconsistency errorDescription
