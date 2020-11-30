module Web.Handler.DeleteUser
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import qualified Core.Interactor.DeleteUser as I
import Core.User
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hDeleteUserHandle :: I.Handle IO
    , hPresent :: Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> UserId -> Application
run Handle {..} uid request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  I.run hDeleteUserHandle authUser uid
  respond hPresent
