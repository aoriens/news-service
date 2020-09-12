module Core.Interactor.GetUsers
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Pagination
import Core.User

data Handle m =
  Handle
    { hGetUsers :: PageSpec -> m [User]
    , hPagerHandle :: PagerHandle
    }

run :: MonadThrow m => Handle m -> PageSpecQuery -> m [User]
run Handle {..} pageQuery = hGetUsers =<< parsePageSpecM hPagerHandle pageQuery
