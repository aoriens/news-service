module Web.Handler.DeleteDraft
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import Core.News
import Web.Application
import qualified Web.Credentials

data Handle =
  Handle
    { hDeleteDraft :: AuthenticatedUser -> NewsVersionId -> IO ()
    , hAuthenticate :: Maybe Credentials -> IO AuthenticatedUser
    , hPresent :: Response
    }

run :: Handle -> NewsVersionId -> Application
run Handle {..} draftId request respond = do
  authUser <-
    hAuthenticate =<< Web.Credentials.getCredentialsFromRequest request
  hDeleteDraft authUser draftId
  respond hPresent
