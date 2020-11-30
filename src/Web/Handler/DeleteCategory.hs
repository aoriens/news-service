module Web.Handler.DeleteCategory
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import Core.Category
import qualified Core.Interactor.DeleteCategory as I
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hDeleteCategoryHandle :: I.Handle IO
    , hPresent :: Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> CategoryId -> Application
run Handle {..} catId request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  I.run hDeleteCategoryHandle authUser catId
  respond hPresent
