module Core.DTO.Image
  ( ImageId(..)
  , Image(..)
  ) where

import qualified Data.ByteString as BS
import Data.Int
import qualified Data.Text as T

newtype ImageId =
  ImageId
    { getImageId :: Int32
    }
  deriving (Eq, Show)

data Image =
  Image
    { imageData :: BS.ByteString
    , imageContentType :: T.Text
    }
  deriving (Eq, Show)
