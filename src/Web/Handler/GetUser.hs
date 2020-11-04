module Web.Handler.GetUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetUser as I
import Core.User
import Web.Application
import Web.Exception

data Handle =
  Handle
    { hGetUserHandle :: I.Handle IO
    , hPresenter :: User -> Response
    }

run :: Handle -> UserId -> Application
run Handle {..} userId' _ respond = do
  user <-
    maybe (throwIO NotFoundException) pure =<< I.run hGetUserHandle userId'
  respond $ hPresenter user
