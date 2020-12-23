-- | Core exception types.
module Core.Exception
  ( CoreException(..)
  , isIncorrectParameterException
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
  = IncorrectParameterException Reason
  -- ^ Obsolete. It's better to use 'Either' with a use-case-specific
  -- failure type to allow the caller to analyze and handle the error.
  | BadCredentialsException Reason
  | AuthenticationRequiredException
   -- ^ Should be used when no permission check is performed;
   -- otherwise 'NoPermissionException' is more appropriate.
  | NoPermissionException Permission ActionName
  | RequestedEntityNotFoundException EntityId
  -- ^ Obsolete. Use 'Maybe' or 'Either' with a use-case-specific
  -- return type instead, so that the caller could handle it.
  | DependentEntitiesNotFoundException [EntityId]
  -- ^ Obsolete, similar to 'IncorrectParameterException'.
  | DisallowedImageContentTypeException ContentType [ContentType]
  deriving (Show, Eq)

instance Exception CoreException

isIncorrectParameterException :: CoreException -> Bool
isIncorrectParameterException IncorrectParameterException {} = True
isIncorrectParameterException _ = False

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
