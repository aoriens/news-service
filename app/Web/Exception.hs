-- | Common exception types available for use in web handlers.
module Web.Exception
  ( BadRequestException(..)
  ) where

import Control.Exception
import Data.Text (Text)

-- | An exception to indicate an issue on the client side. It should
-- be handled as a response with status 4xx.
newtype BadRequestException =
  BadRequestException
    { badRequestExceptionReason :: Text
    }
  deriving (Show)

instance Exception BadRequestException
