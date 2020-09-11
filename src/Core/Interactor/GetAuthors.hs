module Core.Interactor.GetAuthors
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Author
import qualified Core.Authorization as A
import Core.Pagination

data Handle m =
  Handle
    { hGetAuthors :: Page -> m [Author]
    , hAuthHandle :: A.Handle m
    , hPagerHandle :: PagerHandle
    }

run ::
     MonadThrow m => Handle m -> Maybe A.Credentials -> PageQuery -> m [Author]
run h credentials pageQuery = do
  actor <- A.authenticate (hAuthHandle h) credentials
  A.requireAdminPermission actor "get authors"
  page <- pageFromPageQueryM (hPagerHandle h) pageQuery
  hGetAuthors h page
