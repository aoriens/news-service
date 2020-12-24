module Web.Handler.GetAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Author
import Data.Maybe.Util
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hGetAuthor :: AuthenticatedUser -> AuthorId -> m (Maybe Author)
    , hPresent :: Author -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> AuthorId -> GenericApplication m
run Handle {..} authorId' request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  author <-
    fromMaybeM (throwM ResourceNotFoundException) =<<
    hGetAuthor authUser authorId'
  respond $ hPresent author
