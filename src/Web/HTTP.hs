module Web.HTTP
  ( hJSONContentType
  , jsonContentType
  ) where

import qualified Data.ByteString as B
import qualified Network.HTTP.Types as Http

hJSONContentType :: Http.Header
hJSONContentType = (Http.hContentType, jsonContentType)

jsonContentType :: B.ByteString
jsonContentType = "application/json"
