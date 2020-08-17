-- | Common exception types available for use in web handlers.
module Web.Exception
  ( BadRequestException(..)
  , queryParameterException
  ) where

import Control.Exception
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Web.QueryParameter as QP

-- | An exception to indicate an issue on the client side. It should
-- be handled as a response with status 4xx.
newtype BadRequestException =
  BadRequestException
    { badRequestExceptionReason :: Text
    }
  deriving (Show)

instance Exception BadRequestException

queryParameterException :: QP.Failure -> BadRequestException
queryParameterException (QP.MissingKey key) =
  BadRequestException $
  "Parameter '" <> T.decodeLatin1 key <> "' is missing from the request query"
queryParameterException (QP.BadValue key value) =
  BadRequestException $
  mconcat
    [ "Wrong value of parameter '"
    , T.decodeLatin1 key
    , "': '"
    , T.decodeLatin1 (fromMaybe "<missing>" value)
    , "'"
    ]
