module Gateway.Images
  ( getImage
  ) where

import Core.Image
import qualified Database.Images as DImages
import Database.Service.Primitives as DB

getImage :: DB.Handle -> ImageId -> IO (Maybe Image)
getImage h = DB.runTransactionRO h . DImages.selectImage
