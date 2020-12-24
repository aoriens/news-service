module Web.Handler.DeleteTag
  ( run
  , Handle(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Authentication
import Core.Tag
import Web.Application
import qualified Web.Credentials
import Web.Exception

data Handle m =
  Handle
    { hDeleteTag :: AuthenticatedUser -> TagId -> m Bool
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    , hPresent :: Response
    }

run :: MonadThrow m => Handle m -> TagId -> GenericApplication m
run Handle {..} tagId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  isOk <- hDeleteTag authUser tagId
  unless isOk $ throwM ResourceNotFoundException
  respond hPresent
