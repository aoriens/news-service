module Core.Interactor.GetImage
  ( run
  , Handle(..)
  ) where

import Core.DTO.Image

newtype Handle m =
  Handle
    { hGetImage :: ImageId -> m (Maybe Image)
    }

run :: Handle m -> ImageId -> m (Maybe Image)
run = hGetImage
