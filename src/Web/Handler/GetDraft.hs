module Web.Handler.GetDraft
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Authentication
import qualified Core.Interactor.GetDraft as I
import Core.News
import Web.Application
import Web.Credentials
import Web.Exception

data Handle =
  Handle
    { hGetDraftHandle :: I.Handle IO
    , hPresent :: NewsVersion -> Response
    , hAuthenticationHandle :: AuthenticationHandle IO
    }

run :: Handle -> NewsVersionId -> Application
run Handle {..} draftId request respond = do
  authUser <-
    authenticate hAuthenticationHandle =<< getCredentialsFromRequest request
  draft <-
    maybe (throwIO NotFoundException) pure =<<
    I.run hGetDraftHandle authUser draftId
  respond $ hPresent draft
