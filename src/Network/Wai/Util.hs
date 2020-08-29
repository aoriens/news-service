module Network.Wai.Util
  ( runApplicationAndGetStatus
  ) where

import Data.IORef
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

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
