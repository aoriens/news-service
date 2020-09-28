-- | Common exception types available for use in web handlers.
module Web.Exception
  ( WebException(..)
  ) where

import Control.Exception
import Data.Text (Text)
import Data.Word

type Reason = Text

type SupportedMediaTypes = [Text]

type MaxPayloadSize = Word64

data WebException
  = BadRequestException Reason
  | IncorrectParameterException Reason
  | UnsupportedMediaTypeException SupportedMediaTypes
  | PayloadTooLargeException MaxPayloadSize
  | NotFoundException
  | MalformedAuthDataException Reason
  deriving (Show)

instance Exception WebException
