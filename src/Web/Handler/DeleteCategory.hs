module Web.Handler.DeleteCategory
  ( run
  , Handle(..)
  ) where

import Core.Category
import qualified Core.Interactor.DeleteCategory as I
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hDeleteCategoryHandle :: I.Handle IO
    , hPresenter :: Response
    }

run :: Handle -> CategoryId -> Application
run Handle {..} catId request respond = do
  credentials <- getCredentialsFromRequest request
  I.run hDeleteCategoryHandle credentials catId
  respond hPresenter
