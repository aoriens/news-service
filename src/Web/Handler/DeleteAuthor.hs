module Web.Handler.DeleteAuthor
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.Author
import qualified Core.Interactor.DeleteAuthor as I
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hDeleteAuthor :: AuthenticatedUser -> AuthorId -> m (Either I.Failure ())
    , hPresent :: Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> AuthorId -> GenericApplication m
run Handle {..} authorId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  hDeleteAuthor authUser authorId >>= \case
    Left I.UnknownAuthorId -> throwM ResourceNotFoundException
    Right () -> respond hPresent
