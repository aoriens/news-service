{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Gateway.Images
  ( getImage
  ) where

import Core.Image
import Data.Profunctor
import Database as DB
import qualified Hasql.Statement as S
import qualified Hasql.TH as TH

getImage :: DB.Handle -> ImageId -> IO (Maybe Image)
getImage h = DB.runTransaction h . DB.statement selectImage

selectImage :: S.Statement ImageId (Maybe Image)
selectImage =
  dimap
    getImageId
    (fmap $ \(imageData, imageContentType) -> Image {..})
    [TH.maybeStatement|
      select images.content :: bytea, mime_types.value :: varchar
      from images join mime_types using (mime_type_id)
      where image_id = $1 :: integer
    |]
