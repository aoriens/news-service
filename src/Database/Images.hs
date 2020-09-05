{-# LANGUAGE QuasiQuotes #-}

module Database.Images
  ( selectImage
  , createImage
  ) where

import Core.Image
import Data.Profunctor
import qualified Data.Text as T
import Database
import qualified Hasql.TH as TH

selectImage :: Statement ImageId (Maybe Image)
selectImage =
  dimap
    getImageId
    (fmap $ \(imageData, imageContentType) -> Image {..})
    [TH.maybeStatement|
      select images.content :: bytea, mime_types.value :: varchar
      from images join mime_types using (mime_type_id)
      where image_id = $1 :: integer
    |]

createImage :: Image -> Transaction ImageId
createImage image = do
  statement createMimeTypeIfNotFound (imageContentType image)
  statement createImageSt image

createMimeTypeIfNotFound :: Statement T.Text ()
createMimeTypeIfNotFound =
  [TH.resultlessStatement|
    insert into mime_types (value) values ($1 :: varchar) on conflict do nothing
  |]

createImageSt :: Statement Image ImageId
createImageSt =
  dimap
    (\Image {..} -> (imageData, imageContentType))
    ImageId
    [TH.singletonStatement|
    insert into images (content, mime_type_id)
    values (
      $1 :: bytea,
      (select mime_type_id from mime_types where value = $2 :: varchar)
    ) returning image_id :: integer
    |]
