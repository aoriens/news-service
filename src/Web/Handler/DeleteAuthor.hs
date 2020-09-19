module Web.Handler.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Core.Author
import qualified Core.Interactor.DeleteAuthor as I
import qualified Network.Wai as Wai
import Web.Credentials

data Handle =
  Handle
    { hDeleteAuthorHandle :: I.Handle IO
    , hPresenter :: Wai.Response
    }

run :: Handle -> AuthorId -> Wai.Application
run Handle {..} authorIdent request respond = do
  credentials <- getCredentialsFromRequest request
  I.run hDeleteAuthorHandle credentials authorIdent
  respond hPresenter
