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
    , hAuthenticationHandle :: AuthenticationHandle m
    , hAuthorizationHandle :: AuthorizationHandle
    , hDefaultEntityListRange :: PageSpec
    }

run :: MonadThrow m => Handle m -> Maybe Credentials -> UserId -> m ()
run Handle {..} credentials userIdent = do
  actor <- authenticate hAuthenticationHandle credentials
  requireAdminPermission hAuthorizationHandle actor "deleting user"
  either (throwM . failureToException) pure =<<
    hDeleteUser userIdent hDefaultEntityListRange
  where
    failureToException (DependentEntitiesPreventDeletion ids) =
      DependentEntitiesPreventDeletionException (UserEntityId userIdent) ids
    failureToException UnknownUser =
      RequestedEntityNotFoundException $ UserEntityId userIdent

data Failure
  = DependentEntitiesPreventDeletion [EntityId]
  | UnknownUser
