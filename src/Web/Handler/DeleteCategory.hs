module Web.Handler.DeleteCategory
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Authentication
import Core.Category
import qualified Core.Interactor.DeleteCategory as I
import Web.Application
import Web.Credentials
import Web.Exception

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
  I.run hDeleteCategoryHandle authUser catId >>= \case
    Left I.UnknownCategoryId -> throwIO NotFoundException
    Right () -> respond hPresent
