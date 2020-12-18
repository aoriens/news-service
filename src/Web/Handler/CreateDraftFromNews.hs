module Web.Handler.CreateDraftFromNews
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Authentication
import qualified Core.Interactor.CreateDraftFromNews as I
import Core.News
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle =
  Handle
    { hCreateDraftFromNews :: AuthenticatedUser -> NewsId -> IO (Either I.Failure Draft)
    , hPresent :: Draft -> Response
    , hAuthenticate :: Maybe Credentials -> IO AuthenticatedUser
    }

run :: Handle -> NewsId -> Application
run Handle {..} newsId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  hCreateDraftFromNews authUser newsId >>= \case
    Right version -> respond $ hPresent version
    Left I.UnknownNewsId -> throwIO NotFoundException
