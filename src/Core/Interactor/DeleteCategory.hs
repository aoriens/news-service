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
    , hAuthenticationHandle :: AuthenticationHandle m
    , hAuthorizationHandle :: AuthorizationHandle
    , hDefaultEntityListRange :: PageSpec
    }

run :: MonadThrow m => Handle m -> Maybe Credentials -> CategoryId -> m ()
run Handle {..} credentials categoryIdent = do
  actor <- authenticate hAuthenticationHandle credentials
  requireAdminPermission hAuthorizationHandle actor "deleting category"
  either (throwM . failureToException) pure =<<
    hDeleteCategory categoryIdent hDefaultEntityListRange
  where
    failureToException (DependentEntitiesPreventDeletion ids) =
      DependentEntitiesPreventDeletionException
        (CategoryEntityId categoryIdent)
        ids
    failureToException UnknownCategory =
      RequestedEntityNotFoundException $ CategoryEntityId categoryIdent

data Failure
  = DependentEntitiesPreventDeletion [EntityId]
  | UnknownCategory
