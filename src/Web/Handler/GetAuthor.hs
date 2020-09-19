module Web.Handler.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Author
import qualified Core.Interactor.GetAuthor as I
import qualified Network.Wai as Wai
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hGetAuthorHandle :: I.Handle IO
    , hPresenter :: Author -> Wai.Response
    }

run :: Handle -> AuthorId -> Wai.Application
run Handle {..} authorIdent request respond = do
  credentials <- getCredentialsFromRequest request
  author <-
    maybe (throwIO NotFoundException) pure =<<
    I.run hGetAuthorHandle credentials authorIdent
  respond $ hPresenter author
