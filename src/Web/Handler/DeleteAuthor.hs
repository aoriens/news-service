module Web.Handler.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Control.Monad
import Core.Authentication
import Core.Author
import qualified Core.Interactor.DeleteAuthor as I
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hDeleteAuthorHandle :: I.Handle IO
    , hPresent :: Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> AuthorId -> Application
run Handle {..} authorId' request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  r <- I.run hDeleteAuthorHandle authUser authorId'
  unless r $ throwIO NotFoundException
  respond hPresent
