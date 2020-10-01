module Network.Wai.Util
  ( runApplicationAndGetStatus
  ) where

import Data.IORef
import qualified Network.HTTP.Types as Http
import Web.Types

-- | Runs the application and returns its HTTP status after finish.
runApplicationAndGetStatus ::
     Application
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO (ResponseReceived, Http.Status)
runApplicationAndGetStatus app request respond = do
  statusRef <- newIORef (error "The response status must be set here")
  r <-
    app request $ \response -> do
      let (status, _) = responseStatusAndHeaders response
      writeIORef statusRef status
      respond response
  status <- readIORef statusRef
  pure (r, status)
