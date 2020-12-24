module Web.Handler.GetDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import Core.News
import Data.Maybe.Util
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hGetDraft :: AuthenticatedUser -> DraftId -> m (Maybe Draft)
    , hPresent :: Draft -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> DraftId -> GenericApplication m
run Handle {..} draftId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  draft <-
    fromMaybeM (throwM ResourceNotFoundException) =<< hGetDraft authUser draftId
  respond $ hPresent draft
