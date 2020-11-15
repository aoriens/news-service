module Core.Interactor.DeleteCategory
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Category
import Core.EntityId
import Core.Exception
import Core.Pagination

data Handle m =
  Handle
    { hDeleteCategory :: CategoryId -> PageSpec -> m (Either Failure ())
    , hAuthorizationHandle :: AuthorizationHandle
    , hDefaultEntityListRange :: PageSpec
    }

run :: MonadThrow m => Handle m -> AuthenticatedUser -> CategoryId -> m ()
run Handle {..} authUser categoryId' = do
  requireAdminPermission hAuthorizationHandle authUser "deleting category"
  either (throwM . failureToException) pure =<<
    hDeleteCategory categoryId' hDefaultEntityListRange
  where
    failureToException (DependentEntitiesPreventDeletion ids) =
      DependentEntitiesPreventDeletionException
        (CategoryEntityId categoryId')
        ids
    failureToException UnknownCategory =
      RequestedEntityNotFoundException $ CategoryEntityId categoryId'

data Failure
  = DependentEntitiesPreventDeletion [EntityId]
  | UnknownCategory
