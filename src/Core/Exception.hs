-- | Core exception types.
module Core.Exception
  ( QueryException(..)
  ) where

import Control.Exception
import Data.Text (Text)

-- | An exception to designate incorrect input data to an interactor.
newtype QueryException =
  QueryException
    { queryExceptionReason :: Text
    }
  deriving (Show)

instance Exception QueryException
