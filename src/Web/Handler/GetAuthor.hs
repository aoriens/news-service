module Web.Handler.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Author
import qualified Core.Interactor.GetAuthor as I
import Web.Credentials
import Web.Exception
import Web.Types

data Handle =
  Handle
    { hGetAuthorHandle :: I.Handle IO
    , hPresenter :: Author -> Response
    }

run :: Handle -> AuthorId -> Application
run Handle {..} authorIdent request respond = do
  credentials <- getCredentialsFromRequest request
  author <-
    maybe (throwIO NotFoundException) pure =<<
    I.run hGetAuthorHandle credentials authorIdent
  respond $ hPresenter author
