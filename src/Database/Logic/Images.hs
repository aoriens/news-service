{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.Images
  ( getImage
  , imageExists
  , createImage
  , deleteImageIfNotReferenced
  , deleteImagesIfNotReferenced
  ) where

import Core.Image
import Data.Profunctor
import qualified Data.Text as T
import qualified Data.Vector.Unboxed
import Database.Service.Primitives
import qualified Hasql.TH as TH

getImage :: ImageId -> Transaction (Maybe Image)
getImage =
  runStatement $
  dimap
    getImageId
    (fmap $ \(imageData, imageContentType) -> Image {..})
    [TH.maybeStatement|
      select images.content :: bytea, mime_types.value :: varchar
      from images join mime_types using (mime_type_id)
      where image_id = $1 :: integer
    |]

imageExists :: ImageId -> Transaction Bool
imageExists =
  runStatement $
  lmap
    getImageId
    [TH.singletonStatement|
      select (count(*) > 0) :: boolean
      from images
      where image_id = $1 :: integer
    |]

createImage :: Image -> Transaction ImageId
createImage image = do
  createMimeTypeIfNotFound (imageContentType image)
  insertImageRow image

createMimeTypeIfNotFound :: T.Text -> Transaction ()
createMimeTypeIfNotFound =
  runStatement
    [TH.resultlessStatement|
      insert into mime_types (value) values ($1 :: varchar) on conflict do nothing
    |]

insertImageRow :: Image -> Transaction ImageId
insertImageRow =
  runStatement $
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

deleteImageIfNotReferenced :: ImageId -> Transaction ()
deleteImageIfNotReferenced = deleteImagesIfNotReferenced . (: [])

deleteImagesIfNotReferenced :: [ImageId] -> Transaction ()
deleteImagesIfNotReferenced [] = pure ()
deleteImagesIfNotReferenced ids =
  flip runStatement ids $
  lmap
    (Data.Vector.Unboxed.fromList . map getImageId)
    [TH.resultlessStatement|
      delete from images
      where image_id in (
              select * from unnest($1 :: integer[])
              except
              select main_photo_id from news_versions
              except
              select avatar_id from users
              except
              select image_id from news_versions_and_additional_photos_relation
            )

    |]
