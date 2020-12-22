module Web.Handler.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
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
run Handle {..} authorId request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  I.run hDeleteAuthorHandle authUser authorId >>= \case
    Left I.UnknownAuthorId -> throwIO ResourceNotFoundException
    Right () -> respond hPresent
