module Web.Handler.GetCategory
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Category
import qualified Core.Interactor.GetCategory as IGetCategory
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle =
  Handle
    { hGetCategoryHandle :: IGetCategory.Handle IO
    , hPresent :: Category -> Response
    }

run :: Handle -> CategoryId -> Application
run Handle {..} catId _ respond = do
  category <-
    fromMaybeM (throwIO ResourceNotFoundException) =<<
    IGetCategory.run hGetCategoryHandle catId
  respond $ hPresent category
