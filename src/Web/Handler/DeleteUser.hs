module Web.Handler.DeleteUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Control.Monad
import Core.Authentication
import Core.User
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hDeleteUser :: AuthenticatedUser -> UserId -> IO Bool
    , hPresent :: Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> UserId -> Application
run Handle {..} uid request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  r <- hDeleteUser authUser uid
  unless r $ throwIO ResourceNotFoundException
  respond hPresent
