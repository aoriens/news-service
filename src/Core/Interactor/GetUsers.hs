module Core.Interactor.GetUsers
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Pagination
import Core.User

data Handle m =
  Handle
    { hGetUsers :: Page -> m [User]
    , hMaxPageLimit :: PageLimit
    }

run :: MonadThrow m => Handle m -> PageQuery -> m [User]
run Handle {..} pageQuery =
  hGetUsers =<< pageFromPageQueryM hMaxPageLimit pageQuery
