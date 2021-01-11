module Database.Service.Exception where

import Control.Exception
import qualified Data.Text as T
import qualified Hasql.Session as S

data DatabaseException
  = HasqlException S.QueryError
  | DatabaseInternalInconsistencyException T.Text

instance Exception DatabaseException
