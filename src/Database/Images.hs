{-# LANGUAGE QuasiQuotes #-}

module Database.Images
  ( selectImage
  , createImage
  , deleteImageIfNotReferenced
  ) where

import Core.Image
import Data.Profunctor
import qualified Data.Text as T
import Database
import qualified Hasql.TH as TH

selectImage :: ImageId -> Transaction (Maybe Image)
selectImage =
  statement $
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
  createMimeTypeIfNotFound (imageContentType image)
  createImageSt image

createMimeTypeIfNotFound :: T.Text -> Transaction ()
createMimeTypeIfNotFound =
  statement
    [TH.resultlessStatement|
      insert into mime_types (value) values ($1 :: varchar) on conflict do nothing
    |]

createImageSt :: Image -> Transaction ImageId
createImageSt =
  statement $
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

deleteImageIfNotReferenced :: ImageId -> Session ()
deleteImageIfNotReferenced =
  ignoringForeignKeyViolation . transactionRW . deleteImage

deleteImage :: ImageId -> Transaction ()
deleteImage =
  statement $
  lmap
    getImageId
    [TH.resultlessStatement|
      delete from images
      where image_id = $1 :: integer
    |]
