module Web.Handler.DeleteDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import qualified Core.Interactor.DeleteDraft as IDeleteDraft
import Core.News
import Web.Application
import qualified Web.Credentials
import Web.Exception

data Handle m =
  Handle
    { hDeleteDraft :: AuthenticatedUser -> DraftId -> m (Either IDeleteDraft.Failure ())
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    , hPresent :: Response
    }

run :: MonadThrow m => Handle m -> DraftId -> GenericApplication m
run Handle {..} draftId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  hDeleteDraft authUser draftId >>= \case
    Left IDeleteDraft.UnknownDraftId -> throwM ResourceNotFoundException
    Right () -> respond hPresent
