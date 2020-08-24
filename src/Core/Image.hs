module Core.Image
  ( ImageId(..)
  ) where

import Data.Int

newtype ImageId =
  ImageId
    { getImageId :: Int32
    }
  deriving (Eq, Show)
