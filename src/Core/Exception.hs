-- | Core exception types.
module Core.Exception
  ( CoreException(..)
  , isQueryException
  , isBadCredentialsException
  , isNoPermissionException
  , isUserNotIdentifiedException
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
  | AuthenticationRequired
   -- ^ should be used when no permission check is performed;
   -- otherwise 'NoPermissionException' is more appropriate.
  | NoPermissionException Permission ActionName
  | UserNotIdentifiedException ActionName
  | DependentEntitiesPreventDeletionException EntityId [EntityId]
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

isUserNotIdentifiedException :: CoreException -> Bool
isUserNotIdentifiedException UserNotIdentifiedException {} = True
isUserNotIdentifiedException _ = False

isRequestedEntityNotFoundException :: CoreException -> Bool
isRequestedEntityNotFoundException RequestedEntityNotFoundException {} = True
isRequestedEntityNotFoundException _ = False

isDisallowedImageContentTypeException :: CoreException -> Bool
isDisallowedImageContentTypeException DisallowedImageContentTypeException {} =
  True
isDisallowedImageContentTypeException _ = False
