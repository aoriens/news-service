module Web.Handler.GetUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetUser as I
import Core.User
import Web.Exception
import Web.Types

data Handle =
  Handle
    { hGetUserHandle :: I.Handle IO
    , hPresenter :: User -> Response
    }

run :: Handle -> UserId -> Application
run Handle {..} userIdent _ respond = do
  user <-
    maybe (throwIO NotFoundException) pure =<< I.run hGetUserHandle userIdent
  respond $ hPresenter user
