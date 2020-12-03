module Web.Handler.DeleteDraft
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Authentication
import qualified Core.Interactor.DeleteDraft as IDeleteDraft
import Core.News
import Web.Application
import qualified Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hDeleteDraft :: AuthenticatedUser -> NewsVersionId -> IO (Either IDeleteDraft.Failure ())
    , hAuthenticate :: Maybe Credentials -> IO AuthenticatedUser
    , hPresent :: Response
    }

run :: Handle -> NewsVersionId -> Application
run Handle {..} draftId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  hDeleteDraft authUser draftId >>= \case
    Left IDeleteDraft.UnknownDraftId -> throwIO NotFoundException
    Right () -> respond hPresent
