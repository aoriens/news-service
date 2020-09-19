module Core.Interactor.GetNews
  ( getNews
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.News
import Core.Pagination

getNews :: MonadThrow m => Handle m -> PageSpecQuery -> m [News]
getNews Handle {..} pageQuery =
  hGetNews =<< parsePageSpecM hPageSpecParserHandle pageQuery

data Handle m =
  Handle
    { hGetNews :: PageSpec -> m [News]
    , hPageSpecParserHandle :: PageSpecParserHandle
    }
