module Core.Interactor.DeleteUser
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.EntityId
import Core.Exception
import Core.Pagination
import Core.User

data Handle m =
  Handle
    { hDeleteUser :: UserId -> PageSpec -> m (Either Failure ())
    , hAuthHandle :: AuthenticationHandle m
    , hDefaultEntityListRange :: PageSpec
    }

run :: MonadThrow m => Handle m -> Maybe Credentials -> UserId -> m ()
run Handle {..} credentials userIdent = do
  actor <- authenticate hAuthHandle credentials
  requireAdminPermission actor "deleting user"
  either (throwM . failureToException) pure =<<
    hDeleteUser userIdent hDefaultEntityListRange
  where
    failureToException (DependentEntitiesPreventDeletion ids) =
      DependentEntitiesPreventDeletionException (UserEntityId userIdent) ids
    failureToException UnknownUser =
      EntityNotFoundException $ UserEntityId userIdent

data Failure
  = DependentEntitiesPreventDeletion [EntityId]
  | UnknownUser
