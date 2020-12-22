module Web.Handler.GetImage
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Image
import qualified Core.Interactor.GetImage as I
import Data.Maybe.Util
import Web.Application
import qualified Web.Exception as E

data Handle =
  Handle
    { hGetImageHandle :: I.Handle IO
    , hPresent :: Image -> Response
    }

run :: Handle -> ImageId -> Application
run Handle {..} imageId _ respond = do
  optImage <- I.run hGetImageHandle imageId
  image <- fromMaybeM (throwIO E.ResourceNotFoundException) optImage
  respond $ hPresent image
