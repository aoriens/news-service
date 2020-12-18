{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.Delete
  ( deleteDraftAndItsNewsVersion
  , deleteDraftsOfAuthor
  ) where

import Control.Monad
import Core.Author
import Core.Image
import Core.News
import Data.Foldable
import Data.Maybe
import Data.Profunctor
import Database.Logic.Images
import Database.Logic.News.Get
import Database.Service.Primitives
import qualified Hasql.TH as TH

deleteDraftsOfAuthor :: AuthorId -> Transaction ()
deleteDraftsOfAuthor authorId =
  mapM_ deleteDraftAndItsNewsVersion =<< getDraftIdsOfAuthor authorId

deleteDraftAndItsNewsVersion :: DraftId -> Transaction ()
deleteDraftAndItsNewsVersion draftId =
  deleteDraftRow draftId >>= mapM_ deleteNewsVersion

deleteDraftRow :: DraftId -> Transaction (Maybe NewsVersionId)
deleteDraftRow =
  runStatement $
  dimap
    getDraftId
    (fmap NewsVersionId)
    [TH.maybeStatement|
      delete from drafts
      where draft_id = $1 :: integer
      returning news_versions_id :: integer
    |]

deleteNewsVersion :: NewsVersionId -> Transaction ()
deleteNewsVersion vId = do
  additionalPhotoIds <- deleteAdditionalPhotosRelations vId
  mainPhotoId <- getMainPhotoId vId
  deleteImagesIfNotReferenced $ maybeToList mainPhotoId ++ additionalPhotoIds
  deleteNewsVersionRow vId

deleteNewsVersionRow :: NewsVersionId -> Transaction ()
deleteNewsVersionRow =
  runStatement $
  lmap
    getNewsVersionId
    [TH.resultlessStatement|
      delete from news_versions
      where news_version_id = $1 :: integer
    |]

deleteAdditionalPhotosRelations :: NewsVersionId -> Transaction [ImageId]
deleteAdditionalPhotosRelations =
  runStatement $
  dimap
    getNewsVersionId
    (map ImageId . toList)
    [TH.vectorStatement|
      delete from news_versions_and_additional_photos_relation
      where news_version_id = $1 :: integer
      returning image_id :: integer
    |]

getMainPhotoId :: NewsVersionId -> Transaction (Maybe ImageId)
getMainPhotoId =
  runStatement $
  dimap
    getNewsVersionId
    (fmap ImageId . join)
    [TH.maybeStatement|
      select main_photo_id :: integer?
      from news_versions
      where news_version_id = $1 :: integer
    |]
