{-# LANGUAGE RecordWildCards #-}

module Core.Interactor.GetUsers
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.DTO.User
import Core.Pagination

data Handle m =
  Handle
    { hGetUsers :: Page -> m [User]
    , hMaxPageLimit :: PageLimit
    }

run :: MonadThrow m => Handle m -> PageQuery -> m [User]
run Handle {..} pageQuery = hGetUsers =<< fromPageQueryM hMaxPageLimit pageQuery
