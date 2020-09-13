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
  case userIdFromPath $ Wai.pathInfo request of
    Just uid -> do
      credentials <- getCredentialsFromRequest request
      I.run (hDeleteUserHandle h) credentials uid
    Nothing -> pure ()
  respond $ Wai.responseLBS Http.noContent204 [] mempty
