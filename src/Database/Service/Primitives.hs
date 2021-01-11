-- | Database-related basic definitions. The module is recommended for
-- importing from Database.Logic.* modules.
module Database.Service.Primitives
  ( Handle(..)
  , Transaction
  , St.Statement(..)
  , runStatement
  , runTransactionRW
  , DatabaseException(..)
  , S.QueryError
  , databaseInternalInconsistency
  , databaseUnsafeFromJust
  ) where

import Database.Service.Exception
import Database.Service.Transaction
import qualified Hasql.Session as S
import qualified Hasql.Statement as St
