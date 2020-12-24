module Web.Handler.GetUser
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.User
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle m =
  Handle
    { hGetUser :: UserId -> m (Maybe User)
    , hPresent :: User -> Response
    }

run :: MonadThrow m => Handle m -> UserId -> GenericApplication m
run Handle {..} userId _ respond = do
  user <- fromMaybeM (throwM ResourceNotFoundException) =<< hGetUser userId
  respond $ hPresent user
