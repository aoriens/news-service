module Core.Interactor.GetTags
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Pagination
import Core.Tag

data Handle m =
  Handle
    { hGetTags :: PageSpec -> m [Tag]
    , hPageSpecParserHandle :: PageSpecParserHandle
    }

run :: MonadThrow m => Handle m -> PageSpecQuery -> m [Tag]
run Handle {..} pageQuery =
  hGetTags =<< parsePageSpecM hPageSpecParserHandle pageQuery
