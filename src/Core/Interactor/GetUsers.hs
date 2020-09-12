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
    , hPageSpecParserHandle :: PageSpecParserHandle
    }

run :: MonadThrow m => Handle m -> PageSpecQuery -> m [User]
run Handle {..} pageQuery =
  hGetUsers =<< parsePageSpecM hPageSpecParserHandle pageQuery
