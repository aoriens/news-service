-- | Common exception types available for use in web handlers.
module Web.Exception
  ( BadRequestException(..)
  , UnsupportedMediaTypeException(..)
  , PayloadTooLargeException(..)
  , NotFoundException(..)
  ) where

import Control.Exception
import Data.Text (Text)
import Data.Word

-- | An exception to indicate an issue on the client side. It should
-- be handled as a response with status 4xx.
newtype BadRequestException =
  BadRequestException
    { badRequestExceptionReason :: Text
    }
  deriving (Show)

instance Exception BadRequestException

newtype UnsupportedMediaTypeException =
  UnsupportedMediaTypeException
    { supportedMimeTypes :: [Text]
    }
  deriving (Show)

instance Exception UnsupportedMediaTypeException

newtype PayloadTooLargeException =
  PayloadTooLargeException
    { maxPayloadSize :: Word64
    }
  deriving (Show)

instance Exception PayloadTooLargeException

data NotFoundException =
  NotFoundException
  deriving (Show)

instance Exception NotFoundException
