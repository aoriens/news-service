module Network.Wai.Util
  ( simpleResponseStream
  , runApplicationAndGetStatus
  ) where

import qualified Data.ByteString.Builder as LBS
import Data.IORef
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

-- | A helper function to simplify making response bodies from a
-- single IO-produced bytestring builder.
simpleResponseStream ::
     Http.Status -> Http.ResponseHeaders -> IO LBS.Builder -> Wai.Response
simpleResponseStream status headers makeBuilder =
  Wai.responseStream status headers $ \write _ -> makeBuilder >>= write

-- | Runs the application and returns its HTTP status after finish.
runApplicationAndGetStatus ::
     Wai.Application
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO (Wai.ResponseReceived, Http.Status)
runApplicationAndGetStatus app request respond = do
  statusRef <- newIORef (error "The response status must be set here")
  r <-
    app request $ \response -> do
      let (status, _, _) = Wai.responseToStream response
      writeIORef statusRef status
      respond response
  status <- readIORef statusRef
  pure (r, status)
