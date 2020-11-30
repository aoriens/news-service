module Web.Handler.PublishDraft
  ( run
  , Handle(..)
  ) where

import Core.Authentication
import qualified Core.Interactor.PublishDraft as I
import Core.News
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hPublishDraftHandle :: I.Handle IO
    , hPresent :: News -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> NewsVersionId -> Application
run Handle {..} vId request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  news <- I.run hPublishDraftHandle authUser vId
  respond $ hPresent news
