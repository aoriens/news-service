-- | Core exception types.
module Core.Exception
  ( CoreException(..)
  , isQueryException
  , isBadCredentialsException
  , isNoPermissionException
  ) where

import Control.Exception
import Core.EntityId
import Data.Text (Text)

type Reason = Text

-- | An exception to designate incorrect input data to an interactor.
data CoreException
  = QueryException Reason
  | BadCredentialsException Reason
  | NoPermissionException Reason
  | DependentEntitiesPreventDeletionException EntityId [EntityId]
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
