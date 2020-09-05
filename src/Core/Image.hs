module Core.Image
  ( ImageId(..)
  , Image(..)
  ) where

import qualified Data.ByteString as B
import Data.Int
import qualified Data.Text as T

newtype ImageId =
  ImageId
    { getImageId :: Int32
    }
  deriving (Eq, Show)

data Image =
  Image
    { imageData :: B.ByteString
    , imageContentType :: T.Text
    }
  deriving (Eq, Show)
