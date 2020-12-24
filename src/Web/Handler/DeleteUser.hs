module Web.Handler.DeleteUser
  ( run
  , Handle(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Authentication
import Core.User
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hDeleteUser :: AuthenticatedUser -> UserId -> m Bool
    , hPresent :: Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> UserId -> GenericApplication m
run Handle {..} uid request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  r <- hDeleteUser authUser uid
  unless r $ throwM ResourceNotFoundException
  respond hPresent
