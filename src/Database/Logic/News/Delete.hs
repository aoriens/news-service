{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.Delete
  ( deleteDraftAndItsContent
  , deleteDraftButLeaveItsContent
  , unlinkAdditionalPhotosFromVersion
  , unlinkTagsFromVersion
  , deleteNewsVersion
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
import Database.Logic.News.NewsVersionId
import Database.Service.Primitives
import qualified Hasql.TH as TH

deleteDraftsOfAuthor :: AuthorId -> Transaction ()
deleteDraftsOfAuthor authorId =
  mapM_ deleteDraftAndItsContent =<< getDraftIdsOfAuthor authorId

deleteDraftAndItsContent :: DraftId -> Transaction ()
deleteDraftAndItsContent draftId =
  deleteDraftButLeaveItsContent draftId >>= mapM_ deleteNewsVersion

deleteDraftButLeaveItsContent :: DraftId -> Transaction (Maybe NewsVersionId)
deleteDraftButLeaveItsContent =
  runStatement $
  dimap
    getDraftId
    (fmap NewsVersionId)
    [TH.maybeStatement|
      delete from drafts
      where draft_id = $1 :: integer
      returning news_version_id :: integer
    |]

deleteNewsVersion :: NewsVersionId -> Transaction ()
deleteNewsVersion vId = do
  additionalPhotoIds <- unlinkAdditionalPhotosFromVersion vId
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

unlinkAdditionalPhotosFromVersion :: NewsVersionId -> Transaction [ImageId]
unlinkAdditionalPhotosFromVersion =
  runStatement $
  dimap
    getNewsVersionId
    (map ImageId . toList)
    [TH.vectorStatement|
      delete from news_versions_and_additional_photos_relation
      where news_version_id = $1 :: integer
      returning image_id :: integer
    |]

unlinkTagsFromVersion :: NewsVersionId -> Transaction ()
unlinkTagsFromVersion =
  runStatement $
  lmap
    getNewsVersionId
    [TH.resultlessStatement|
      delete from news_versions_and_tags_relation
      where news_version_id = $1 :: integer
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
