module Web.Handler.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Authentication
import Core.Author
import qualified Core.Interactor.GetAuthor as I
import Data.Maybe.Util
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hGetAuthorHandle :: I.Handle IO
    , hPresent :: Author -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> AuthorId -> Application
run Handle {..} authorId' request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  author <-
    fromMaybeM (throwIO ResourceNotFoundException) =<<
    I.run hGetAuthorHandle authUser authorId'
  respond $ hPresent author
