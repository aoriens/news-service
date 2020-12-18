module Web.Handler.PublishDraft
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Authentication
import qualified Core.Interactor.PublishDraft as I
import Core.News
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hPublishDraftHandle :: I.Handle IO
    , hPresent :: News -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> DraftId -> Application
run Handle {..} vId request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  I.run hPublishDraftHandle authUser vId >>= \case
    Left I.UnknownDraftId -> throwIO NotFoundException
    Right news -> respond $ hPresent news
