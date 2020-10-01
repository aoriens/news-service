module Web.Handler.GetImage
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Image
import qualified Core.Interactor.GetImage as I
import Web.Application
import qualified Web.Exception as E

data Handle =
  Handle
    { hGetImageHandle :: I.Handle IO
    , hPresenter :: Image -> Response
    }

run :: Handle -> ImageId -> Application
run Handle {..} imageId _ respond = do
  optImage <- I.run hGetImageHandle imageId
  image <- maybe (throwIO E.NotFoundException) pure optImage
  respond $ hPresenter image
