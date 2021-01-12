-- | Database-related basic definitions. The module is recommended for
-- importing from Database.Logic.* modules.
module Database.Service.Primitives
  ( Transaction
  , St.Statement(..)
  , runStatement
  ) where

import Database.Service.Transaction
import qualified Hasql.Statement as St
