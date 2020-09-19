module Web.Handler.GetUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetUser as I
import Core.User
import qualified Network.Wai as Wai
import Web.Exception

data Handle =
  Handle
    { hGetUserHandle :: I.Handle IO
    , hPresenter :: User -> Wai.Response
    }

run :: Handle -> UserId -> Wai.Application
run Handle {..} userIdent _ respond = do
  user <-
    maybe (throwIO NotFoundException) pure =<< I.run hGetUserHandle userIdent
  respond $ hPresenter user
