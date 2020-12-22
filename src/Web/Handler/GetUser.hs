module Web.Handler.GetUser
  ( run
  , Handle(..)
  ) where

import Control.Exception
import qualified Core.Interactor.GetUser as I
import Core.User
import Data.Maybe.Util
import Web.Application
import Web.Exception

data Handle =
  Handle
    { hGetUserHandle :: I.Handle IO
    , hPresent :: User -> Response
    }

run :: Handle -> UserId -> Application
run Handle {..} userId _ respond = do
  user <-
    fromMaybeM (throwIO ResourceNotFoundException) =<<
    I.run hGetUserHandle userId
  respond $ hPresent user
