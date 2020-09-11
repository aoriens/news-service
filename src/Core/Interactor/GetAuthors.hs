module Core.Interactor.GetAuthors
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.Pagination

data Handle m =
  Handle
    { hGetAuthors :: Page -> m [Author]
    , hAuthHandle :: AuthenticationHandle m
    , hPagerHandle :: PagerHandle
    }

run :: MonadThrow m => Handle m -> Maybe Credentials -> PageQuery -> m [Author]
run h credentials pageQuery = do
  actor <- authenticate (hAuthHandle h) credentials
  requireAdminPermission actor "get authors"
  page <- pageFromPageQueryM (hPagerHandle h) pageQuery
  hGetAuthors h page
