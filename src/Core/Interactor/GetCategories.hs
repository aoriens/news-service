module Core.Interactor.GetCategories
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Category
import Core.Pagination

data Handle m =
  Handle
    { hPageSpecParserHandle :: PageSpecParserHandle
    , hGetCategories :: PageSpec -> m [Category]
    }

run :: MonadThrow m => Handle m -> PageSpecQuery -> m [Category]
run Handle {..} pageSpecQuery = do
  pageSpec <- parsePageSpecM hPageSpecParserHandle pageSpecQuery
  hGetCategories pageSpec
