module Web.Handler.GetUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetUser as I
import Core.User
import qualified Network.Wai as Wai
import Web.Exception
import Web.PathParameter

data Handle =
  Handle
    { hGetUserHandle :: I.Handle IO
    , hPresenter :: User -> Wai.Response
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  userIdent <- getUserIdFromPath (Wai.pathInfo request)
  user <-
    maybe (throwIO NotFoundException) pure =<< I.run hGetUserHandle userIdent
  respond $ hPresenter user
