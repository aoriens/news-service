module Gateway.Images
  ( getImage
  ) where

import Core.Image
import Database as DB
import qualified Database.Images as DImages

getImage :: DB.Handle -> ImageId -> IO (Maybe Image)
getImage h = DB.runTransactionRO h . DB.statement DImages.selectImage
