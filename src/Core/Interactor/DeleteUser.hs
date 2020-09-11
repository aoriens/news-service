module Core.Interactor.DeleteUser
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.User

data Handle m =
  Handle
    { hDeleteUser :: UserId -> m ()
    , hAuthHandle :: AuthenticationHandle m
    }

run :: MonadThrow m => Handle m -> Maybe Credentials -> UserId -> m ()
run Handle {..} credentials userIdent = do
  actor <- authenticate hAuthHandle credentials
  requireAdminPermission actor "deleting user"
  hDeleteUser userIdent
