module Web.Handler.CreateDraftFromNews
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authentication
import qualified Core.Interactor.CreateDraftFromNews as I
import Core.News
import Web.Application
import Web.Credentials hiding (Credentials)
import Web.Exception

data Handle m =
  Handle
    { hCreateDraftFromNews :: AuthenticatedUser -> NewsId -> m (Either I.Failure Draft)
    , hPresent :: Draft -> Response
    , hAuthenticate :: Maybe Credentials -> m AuthenticatedUser
    }

run :: MonadThrow m => Handle m -> NewsId -> GenericApplication m
run Handle {..} newsId request respond = do
  authUser <- hAuthenticate =<< getCredentialsFromRequest request
  hCreateDraftFromNews authUser newsId >>= \case
    Right version -> respond $ hPresent version
    Left I.UnknownNewsId -> throwM ResourceNotFoundException
