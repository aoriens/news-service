module Web.Handler.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.DeleteAuthor as I
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import Web.Credentials
import Web.PathParameter

newtype Handle =
  Handle
    { hDeleteAuthorHandle :: I.Handle IO
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  case authorIdFromPath $ Wai.pathInfo request of
    Just authorIdent -> do
      credentials <- getCredentialsFromRequest request
      I.run hDeleteAuthorHandle credentials authorIdent
    Nothing -> pure ()
  respond $ Wai.responseLBS Http.noContent204 [] mempty
