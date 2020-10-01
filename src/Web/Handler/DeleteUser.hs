module Web.Handler.DeleteUser
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.DeleteUser as I
import Core.User
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hDeleteUserHandle :: I.Handle IO
    , hPresenter :: Response
    }

run :: Handle -> UserId -> Application
run Handle {..} uid request respond = do
  credentials <- getCredentialsFromRequest request
  I.run hDeleteUserHandle credentials uid
  respond hPresenter
