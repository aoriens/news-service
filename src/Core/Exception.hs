-- | Core exception types.
module Core.Exception
  ( CoreException(..)
  , isQueryException
  , isBadCredentialsException
  , isNoPermissionException
  , isNoPermissionExceptionWithPermission
  , isRequestedEntityNotFoundException
  , isDisallowedImageContentTypeException
  ) where

import Control.Exception
import Core.EntityId
import Core.Permission
import Data.Text (Text)

type Reason = Text

type ActionName = Text

type ContentType = Text

-- | An exception to designate incorrect input data to an interactor.
data CoreException
  = QueryException Reason
  | BadCredentialsException Reason
  | AuthenticationRequiredException
   -- ^ should be used when no permission check is performed;
   -- otherwise 'NoPermissionException' is more appropriate.
  | NoPermissionException Permission ActionName
  | RequestedEntityNotFoundException EntityId
  | DependentEntitiesNotFoundException [EntityId]
  | DisallowedImageContentTypeException ContentType [ContentType]
  deriving (Show, Eq)

instance Exception CoreException

isQueryException :: CoreException -> Bool
isQueryException QueryException {} = True
isQueryException _ = False

isBadCredentialsException :: CoreException -> Bool
isBadCredentialsException BadCredentialsException {} = True
isBadCredentialsException _ = False

isNoPermissionException :: CoreException -> Bool
isNoPermissionException NoPermissionException {} = True
isNoPermissionException _ = False

isNoPermissionExceptionWithPermission :: Permission -> CoreException -> Bool
isNoPermissionExceptionWithPermission perm e
  | NoPermissionException p _ <- e = p == perm
  | otherwise = False

isRequestedEntityNotFoundException :: CoreException -> Bool
isRequestedEntityNotFoundException RequestedEntityNotFoundException {} = True
isRequestedEntityNotFoundException _ = False

isDisallowedImageContentTypeException :: CoreException -> Bool
isDisallowedImageContentTypeException DisallowedImageContentTypeException {} =
  True
isDisallowedImageContentTypeException _ = False
