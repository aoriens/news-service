module Web.Handler.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Core.Author
import qualified Core.Interactor.DeleteAuthor as I
import Web.Credentials
import Web.Types

data Handle =
  Handle
    { hDeleteAuthorHandle :: I.Handle IO
    , hPresenter :: Response
    }

run :: Handle -> AuthorId -> Application
run Handle {..} authorIdent request respond = do
  credentials <- getCredentialsFromRequest request
  I.run hDeleteAuthorHandle credentials authorIdent
  respond hPresenter
