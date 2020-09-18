module Web.Handler.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetAuthor as I
import qualified Network.Wai as Wai
import Web.Credentials
import Web.Exception
import Web.PathParameter
import Web.Representation.Author
import Web.RepresentationBuilder

data Handle =
  Handle
    { hGetAuthorHandle :: I.Handle IO
    , hPresenterHandle :: RepBuilderHandle
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  credentials <- getCredentialsFromRequest request
  authorIdent <- getAuthorIdFromPath (Wai.pathInfo request)
  author <-
    maybe (throwIO NotFoundException) pure =<<
    I.run hGetAuthorHandle credentials authorIdent
  respond $ runRepBuilder hPresenterHandle $ authorRepresentation author
