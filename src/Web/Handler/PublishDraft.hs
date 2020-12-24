module Web.Handler.PublishDraft
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import qualified Core.Interactor.PublishDraft as I
import Core.News
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hPublishDraft :: AuthenticatedUser -> DraftId -> m (Either I.Failure I.Success)
    , hPresent :: I.Success -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> DraftId -> GenericApplication m
run Handle {..} vId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  hPublishDraft authUser vId >>= \case
    Left I.UnknownDraftId -> throwM ResourceNotFoundException
    Right success -> respond $ hPresent success
