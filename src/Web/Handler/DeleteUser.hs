module Web.Handler.DeleteUser
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.DeleteUser as I
import qualified Network.Wai as Wai
import Web.Credentials
import Web.PathParameter

data Handle =
  Handle
    { hDeleteUserHandle :: I.Handle IO
    , hPresenter :: Wai.Response
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  uid <- getUserIdFromPath $ Wai.pathInfo request
  credentials <- getCredentialsFromRequest request
  I.run hDeleteUserHandle credentials uid
  respond hPresenter
