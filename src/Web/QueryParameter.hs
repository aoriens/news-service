-- | Reading parameters from URI query.
module Web.QueryParameter
  ( QueryParameter(..)
  , lookupQueryParameter
  , requireQueryParameter
  ) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as Http
import Text.Read

type Key = BS.ByteString

-- | Searches and parses the first value matching the key. A missing
--  value is considered an error.
requireQueryParameter ::
     QueryParameter a => Key -> Http.Query -> Either T.Text a
requireQueryParameter key query = do
  raw <- maybe (missingKeyError key) Right $ lookup key query
  maybe (badFormatError key) Right (parseQueryParameter raw)

-- | Searches and parses the first value matching the key. A missing
-- value results in @Right Nothing@.
lookupQueryParameter ::
     QueryParameter a => Key -> Http.Query -> Either T.Text (Maybe a)
lookupQueryParameter key query =
  case lookup key query of
    Nothing -> Right Nothing
    Just raw ->
      maybe (badFormatError key) (Right . Just) (parseQueryParameter raw)

missingKeyError :: Key -> Either T.Text a
missingKeyError key =
  Left $
  "Parameter '" <> T.decodeLatin1 key <> "' is missing from the request query"

badFormatError :: Key -> Either T.Text a
badFormatError key =
  Left $ "Wrong value of parameter '" <> T.decodeLatin1 key <> "'"

-- | Types that can be parsed from an HTTP request query item.
class QueryParameter a
  -- | Parses a value from an optional bytestring value. It should
  -- return Nothing in case of parse error.
  where
  parseQueryParameter :: Maybe BS.ByteString -> Maybe a

instance QueryParameter Int where
  parseQueryParameter = readFromChar8ByteString

instance QueryParameter Int32 where
  parseQueryParameter = readFromChar8ByteString

readFromChar8ByteString :: Read a => Maybe BS.ByteString -> Maybe a
readFromChar8ByteString = fmap BS8.unpack >=> readMaybe
