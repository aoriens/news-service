module WaiUtil
  ( simpleResponseStream
  ) where

import qualified Data.ByteString.Builder as LBS
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

-- | A helper function to simplify making response bodies from a
-- single IO-produced bytestring builder.
simpleResponseStream ::
     Http.Status -> Http.ResponseHeaders -> IO LBS.Builder -> Wai.Response
simpleResponseStream status headers makeBuilder =
  Wai.responseStream status headers $ \write _ -> makeBuilder >>= write
