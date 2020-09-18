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
  authorIdent <- getAuthorIdFromPath $ Wai.pathInfo request
  credentials <- getCredentialsFromRequest request
  I.run hDeleteAuthorHandle credentials authorIdent
  respond $ Wai.responseLBS Http.noContent204 [] mempty
