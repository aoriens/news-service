module Web.Handler.PublishDraft
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.PublishDraft as I
import Core.News
import Web.Application
import Web.Credentials

data Handle =
  Handle
    { hPublishDraftHandle :: I.Handle IO
    , hPresenter :: News -> Response
    }

run :: Handle -> NewsVersionId -> Application
run Handle {..} vId request respond = do
  creds <- getCredentialsFromRequest request
  news <- I.run hPublishDraftHandle creds vId
  respond $ hPresenter news
