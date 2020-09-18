module Web.Handler.DeleteUser
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.DeleteUser as I
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.Credentials
import Web.PathParameter

newtype Handle =
  Handle
    { hDeleteUserHandle :: I.Handle IO
    }

run :: Handle -> Wai.Application
run h request respond = do
  uid <- getUserIdFromPath $ Wai.pathInfo request
  credentials <- getCredentialsFromRequest request
  I.run (hDeleteUserHandle h) credentials uid
  respond $ Wai.responseLBS Http.noContent204 [] mempty
