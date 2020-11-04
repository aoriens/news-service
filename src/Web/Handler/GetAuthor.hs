module Web.Handler.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Author
import qualified Core.Interactor.GetAuthor as I
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hGetAuthorHandle :: I.Handle IO
    , hPresenter :: Author -> Response
    }

run :: Handle -> AuthorId -> Application
run Handle {..} authorId' request respond = do
  credentials <- getCredentialsFromRequest request
  author <-
    maybe (throwIO NotFoundException) pure =<<
    I.run hGetAuthorHandle credentials authorId'
  respond $ hPresenter author
