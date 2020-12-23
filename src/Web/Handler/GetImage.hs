module Web.Handler.GetImage
  ( run
  , Handle(..)
  ) where

import Control.Monad.Catch
import Core.Image
import Data.Maybe.Util
import Web.Application
import qualified Web.Exception as E

data Handle m =
  Handle
    { hGetImage :: ImageId -> m (Maybe Image)
    , hPresent :: Image -> Response
    }

run :: MonadThrow m => Handle m -> ImageId -> GenericApplication m
run Handle {..} imageId _ respond = do
  optImage <- hGetImage imageId
  image <- fromMaybeM (throwM E.ResourceNotFoundException) optImage
  respond $ hPresent image
