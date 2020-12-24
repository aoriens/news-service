module Web.Handler.DeleteCategory
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Category
import qualified Core.Interactor.DeleteCategory as I
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hDeleteCategory :: AuthenticatedUser -> CategoryId -> m (Either I.Failure ())
    , hPresent :: Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> CategoryId -> GenericApplication m
run Handle {..} catId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  hDeleteCategory authUser catId >>= \case
    Left I.UnknownCategoryId -> throwM ResourceNotFoundException
    Right () -> respond hPresent
