-- | Core exception types.
module Core.Exception
  ( CoreException(..)
  ) where

import Control.Exception
import Data.Text (Text)

type Reason = Text

-- | An exception to designate incorrect input data to an interactor.
data CoreException
  = QueryException Reason
  | BadCredentialsException Reason
  | NoPermissionException Reason
  deriving (Show)

instance Exception CoreException
