{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.Create
  ( createDraft
  , makeDraftIntoNews
  , copyDraftFromNews
  ) where

import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Author
import Core.Category
import Core.Deletable
import Core.EntityId
import Core.Image
import qualified Core.Interactor.CreateDraft as ICreateDraft
import Core.News
import Core.Tag
import Data.Foldable
import qualified Data.HashSet as Set
import Data.Profunctor
import qualified Data.Text as T
import Data.Time
import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.Images
import Database.Logic.News.Delete
import Database.Logic.News.Get
import Database.Logic.Tags
import Database.Service.Primitives
import qualified Hasql.TH as TH

createDraft ::
     ICreateDraft.CreateDraftCommand
  -> Transaction (Either ICreateDraft.CreateDraftFailure Draft)
createDraft ICreateDraft.CreateDraftCommand {..} =
  runExceptT $ do
    author <- getExistingEntityBy selectAuthorById cdcAuthorId
    category <- getExistingCategoryIfJust cdcCategoryId
    nvTags <- getExistingTags
    nvMainPhotoId <- mapM createOrGetExistingImage cdcMainPhoto
    (draftId, nvId) <-
      lift . createDraftRow $
      InsertDraftRowCommand
        { idcTitle = cdcTitle
        , idcText = cdcText
        , idcAuthorId = cdcAuthorId
        , idcCategoryId = cdcCategoryId
        , idcMainPhotoId = nvMainPhotoId
        }
    nvAdditionalPhotoIds <- createOrGetExistingAdditionalPhotos
    lift $ addPhotosToVersion nvId nvAdditionalPhotoIds
    lift $ addTagsToVersion nvId cdcTagIds
    pure
      Draft
        { draftId
        , draftContent =
            NewsVersion
              { nvTitle = cdcTitle
              , nvText = cdcText
              , nvAuthor = Existing author
              , nvCategory = category
              , nvMainPhotoId
              , nvAdditionalPhotoIds
              , nvTags
              }
        }
  where
    getExistingTags =
      Set.fromList <$> mapM (getExistingEntityBy findTagById) (toList cdcTagIds)
    createOrGetExistingAdditionalPhotos =
      Set.fromList <$> mapM createOrGetExistingImage cdcAdditionalPhotos

getExistingEntityBy ::
     IsEntityId id
  => (id -> Transaction (Maybe entity))
  -> id
  -> ExceptT ICreateDraft.CreateDraftFailure Transaction entity
getExistingEntityBy getOptEntity id' = do
  optEntity <- lift $ getOptEntity id'
  case optEntity of
    Nothing -> failWithEntityNotFound id'
    Just entity -> pure entity

getExistingCategoryIfJust ::
     Maybe CategoryId
  -> ExceptT ICreateDraft.CreateDraftFailure Transaction (Maybe Category)
getExistingCategoryIfJust =
  maybe (pure Nothing) (fmap Just . getExistingEntityBy selectCategory)

createOrGetExistingImage ::
     Either ImageId Image
  -> ExceptT ICreateDraft.CreateDraftFailure Transaction ImageId
createOrGetExistingImage =
  \case
    Right image -> lift $ createImage image
    Left imageId' -> do
      exists <- lift $ imageExists imageId'
      if exists
        then pure imageId'
        else failWithEntityNotFound imageId'

failWithEntityNotFound ::
     IsEntityId id
  => id
  -> ExceptT ICreateDraft.CreateDraftFailure Transaction a
failWithEntityNotFound objId =
  throwE $ ICreateDraft.CDUnknownEntityId [toEntityId objId]

createDraftRow :: InsertDraftRowCommand -> Transaction (DraftId, NewsVersionId)
createDraftRow =
  runStatement $
  dimap
    (\InsertDraftRowCommand {..} ->
       ( idcTitle
       , idcText
       , getAuthorId idcAuthorId
       , getCategoryId <$> idcCategoryId
       , getImageId <$> idcMainPhotoId))
    (DraftId *** NewsVersionId)
    [TH.singletonStatement|
      with draft_content as (
        insert into news_versions (
          title,
          body,
          author_id,
          category_id,
          main_photo_id
        ) values (
          $1 :: varchar,
          $2 :: varchar,
          $3 :: integer,
          $4 :: integer?,
          $5 :: integer?
        ) returning news_version_id
      )
      insert into drafts (news_version_id)
      select * from draft_content
      returning draft_id :: integer, news_version_id :: integer
    |]

data InsertDraftRowCommand =
  InsertDraftRowCommand
    { idcTitle :: T.Text
    , idcText :: T.Text
    , idcAuthorId :: AuthorId
    , idcCategoryId :: Maybe CategoryId
    , idcMainPhotoId :: Maybe ImageId
    }

addPhotosToVersion :: NewsVersionId -> Set.HashSet ImageId -> Transaction ()
addPhotosToVersion = mapM_ . insertVersionAndAdditionalPhotoAssociation

insertVersionAndAdditionalPhotoAssociation ::
     NewsVersionId -> ImageId -> Transaction ()
insertVersionAndAdditionalPhotoAssociation =
  curry . runStatement $
  lmap
    (getNewsVersionId *** getImageId)
    [TH.resultlessStatement|
      insert into news_versions_and_additional_photos_relation (
        news_version_id,
        image_id
      ) values (
        $1 :: integer,
        $2 :: integer
      ) on conflict do nothing
    |]

addTagsToVersion :: NewsVersionId -> Set.HashSet TagId -> Transaction ()
addTagsToVersion = mapM_ . insertVersionAndTagAssociation

insertVersionAndTagAssociation :: NewsVersionId -> TagId -> Transaction ()
insertVersionAndTagAssociation =
  curry . runStatement $
  lmap
    (getNewsVersionId *** getTagId)
    [TH.resultlessStatement|
      insert into news_versions_and_tags_relation (
        news_version_id,
        tag_id
      ) values (
        $1 :: integer,
        $2 :: integer
      ) on conflict do nothing
    |]

makeDraftIntoNews :: DraftId -> Day -> Transaction News
makeDraftIntoNews draftId day = do
  contentId <-
    databaseUnsafeFromJust ("Cannot find draft_id=" <> T.pack (show draftId)) =<<
    deleteDraftButLeaveItsContent draftId
  newsId' <- insertNews contentId day
  getNews newsId' >>=
    databaseUnsafeFromJust
      ("Cannot find news just created: news_id=" <> T.pack (show newsId'))

insertNews :: NewsVersionId -> Day -> Transaction NewsId
insertNews =
  curry . runStatement $
  dimap
    (first getNewsVersionId)
    NewsId
    [TH.singletonStatement|
      insert into news (
        news_version_id,
        "date"
      ) values (
        $1 :: integer,
        $2 :: date
      ) returning news_id :: integer
    |]

copyDraftFromNews :: NewsId -> Transaction Draft
copyDraftFromNews newsId = do
  (draftId, contentId) <- copyDraftRowFromNews newsId
  copyAdditionalImagesFromNewsToVersion newsId contentId
  copyTagsFromNewsToVersion newsId contentId
  getDraft draftId >>=
    databaseUnsafeFromJust "Cannot find a just inserted draft row"

copyDraftRowFromNews :: NewsId -> Transaction (DraftId, NewsVersionId)
copyDraftRowFromNews =
  runStatement $
  dimap
    getNewsId
    (DraftId *** NewsVersionId)
    [TH.singletonStatement|
      with draft_content as (
        insert into news_versions
              (title, body, author_id, category_id, main_photo_id)
        select title, body, author_id, category_id, main_photo_id
        from news_versions
             join news using (news_version_id)
        where news_id = $1 :: integer
        returning news_version_id
      )
      insert into drafts (news_version_id, created_from_news_id)
      select news_version_id, $1 :: integer
      from draft_content
      returning draft_id :: integer, news_version_id :: integer
    |]

copyAdditionalImagesFromNewsToVersion ::
     NewsId -> NewsVersionId -> Transaction ()
copyAdditionalImagesFromNewsToVersion =
  curry . runStatement $
  lmap
    (getNewsId *** getNewsVersionId)
    [TH.resultlessStatement|
      insert into news_versions_and_additional_photos_relation
        (news_version_id, image_id)
      select $2 :: integer, image_id
      from news_versions_and_additional_photos_relation
           join news using (news_version_id)
      where news_id = $1 :: integer
    |]

copyTagsFromNewsToVersion :: NewsId -> NewsVersionId -> Transaction ()
copyTagsFromNewsToVersion =
  curry . runStatement $
  lmap
    (getNewsId *** getNewsVersionId)
    [TH.resultlessStatement|
      insert into news_versions_and_tags_relation
        (news_version_id, tag_id)
      select $2 :: integer, tag_id
      from news_versions_and_tags_relation
           join news using (news_version_id)
      where news_id = $1 :: integer
    |]
