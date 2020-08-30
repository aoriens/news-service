{-# LANGUAGE RecordWildCards #-}

module Core.Interactor.DeleteUser
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import qualified Core.Authorization as Auth
import Core.DTO.User

data Handle m =
  Handle
    { hDeleteUser :: UserId -> m ()
    , hAuthHandle :: Auth.Handle m
    }

run :: MonadThrow m => Handle m -> Maybe Auth.Credentials -> UserId -> m ()
run Handle {..} credentials userIdent = do
  actor <- Auth.authenticate hAuthHandle credentials
  actor `Auth.requiresAdminPermission` hDeleteUser userIdent
