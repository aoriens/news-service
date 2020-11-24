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
    , hAuthorizationHandle :: AuthorizationHandle
    , hDefaultEntityListRange :: PageSpec
    }

run :: MonadThrow m => Handle m -> AuthenticatedUser -> UserId -> m ()
run Handle {..} authUser userId' = do
  requirePermission
    hAuthorizationHandle
    AdminPermission
    authUser
    "deleting user"
  either (throwM . failureToException) pure =<<
    hDeleteUser userId' hDefaultEntityListRange
  where
    failureToException (DependentEntitiesPreventDeletion ids) =
      DependentEntitiesPreventDeletionException (UserEntityId userId') ids
    failureToException UnknownUser =
      RequestedEntityNotFoundException $ UserEntityId userId'

data Failure
  = DependentEntitiesPreventDeletion [EntityId]
  | UnknownUser
