module Web.HTTP
  ( hJSONContentType
  , jsonContentType
  ) where

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as Http

hJSONContentType :: Http.Header
hJSONContentType = (Http.hContentType, jsonContentType)

jsonContentType :: BS.ByteString
jsonContentType = "application/json"
