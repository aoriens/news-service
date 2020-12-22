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
    { hDeleteDraft :: AuthenticatedUser -> DraftId -> IO (Either IDeleteDraft.Failure ())
    , hAuthenticate :: Maybe Credentials -> IO AuthenticatedUser
    , hPresent :: Response
    }

run :: Handle -> DraftId -> Application
run Handle {..} draftId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  hDeleteDraft authUser draftId >>= \case
    Left IDeleteDraft.UnknownDraftId -> throwIO ResourceNotFoundException
    Right () -> respond hPresent
