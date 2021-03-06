{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.Create
  ( createDraft
  , updateDraft
  , makeDraftIntoNews
  , overwriteNewsWithDraft
  , copyDraftFromNews
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Author
import Core.Category
import Core.EntityId
import Core.Image
import qualified Core.Interactor.CreateDraft as ICreateDraft
import qualified Core.Interactor.PublishDraft as IPublishDraft
import qualified Core.Interactor.UpdateDraft as IUpdateDraft
import Core.News
import Core.Tag
import Data.Foldable
import qualified Data.HashSet as Set
import Data.Maybe
import Data.Maybe.Util
import Data.Profunctor
import qualified Data.Text as T
import Data.Text.Show
import Data.Time
import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.Images
import Database.Logic.News.Delete
import Database.Logic.News.Get
import Database.Logic.News.NewsVersionId
import Database.Logic.Tags
import Database.Service.Exception
import Database.Service.Primitives
import qualified Database.Service.SQLBuilder as Sql
import qualified Database.Service.SQLBuilders as Sql
import qualified Hasql.Decoders as D
import qualified Hasql.TH as TH

createDraft ::
     ICreateDraft.CreateDraftCommand
  -> Transaction (Either ICreateDraft.CreateDraftFailure Draft)
createDraft ICreateDraft.CreateDraftCommand {..} =
  runExceptT . withExceptT ICreateDraft.CDUnknownEntityId $ do
    entityMustExistBy authorExists cdcAuthorId
    mapM_ (entityMustExistBy categoryExists) cdcCategoryId
    mapM_ (entityMustExistBy tagExists) cdcTagIds
    mainPhotoId <- mapM createOrGetExistingImage cdcMainPhoto
    (draftId, nvId) <-
      lift . createDraftRow $
      InsertDraftRowCommand
        { idcTitle = cdcTitle
        , idcText = cdcText
        , idcAuthorId = cdcAuthorId
        , idcCategoryId = cdcCategoryId
        , idcMainPhotoId = mainPhotoId
        }
    additionalPhotoIds <- createOrGetExistingAdditionalPhotos
    lift $ addPhotosToVersion nvId additionalPhotoIds
    lift $ addTagsToVersion nvId cdcTagIds
    lift $
      getDraft draftId >>=
      databaseUnsafeFromJust
        ("Cannot find a just created draft_id = " <> showAsText draftId)
  where
    createOrGetExistingAdditionalPhotos =
      Set.fromList <$> mapM createOrGetExistingImage cdcAdditionalPhotos

updateDraft ::
     DraftId
  -> IUpdateDraft.UpdateDraftRequest
  -> Transaction (Either IUpdateDraft.UpdateDraftFailure Draft)
updateDraft draftId IUpdateDraft.UpdateDraftRequest {..} =
  runExceptT . withExceptT IUpdateDraft.UDUnknownEntityIds $ do
    versionId <-
      fromMaybeM (unknownEntityFailure draftId) =<<
      lift (getDraftContentId draftId)
    mapM_ (entityMustExistBy categoryExists) $ join udrCategory
    mapM_ (mapM_ (entityMustExistBy tagExists)) udrTags
    mainPhotoId <- mapM (mapM createOrGetExistingImage) udrMainPhoto
    lift . updateVersionRow versionId $
      UpdateVersionRowCommand
        { uvrTitle = udrTitle
        , uvrText = udrText
        , uvrCategory = udrCategory
        , uvrMainPhoto = mainPhotoId
        }
    mapM_ (updateAdditionalPhotos versionId) udrAdditionalPhotos
    mapM_ (lift . setTagsToVersion versionId) udrTags
    lift (getDraft draftId) >>= fromMaybeM (unknownEntityFailure draftId)
  where
    updateAdditionalPhotos versionId images =
      lift . setPhotosToVersion versionId . Set.fromList =<<
      mapM createOrGetExistingImage images

type UnknownEntityIds = [EntityId]

entityMustExistBy ::
     IsEntityId id
  => (id -> Transaction Bool)
  -> id
  -> ExceptT UnknownEntityIds Transaction ()
entityMustExistBy checkIfExists id' =
  lift (checkIfExists id') >>= (`unless` unknownEntityFailure id')

createOrGetExistingImage ::
     Either ImageId Image -> ExceptT UnknownEntityIds Transaction ImageId
createOrGetExistingImage =
  \case
    Right image -> lift $ createImage image
    Left imageId' -> do
      exists <- lift $ imageExists imageId'
      if exists
        then pure imageId'
        else unknownEntityFailure imageId'

unknownEntityFailure ::
     IsEntityId id => id -> ExceptT UnknownEntityIds Transaction a
unknownEntityFailure objId = throwE [toEntityId objId]

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

updateVersionRow :: NewsVersionId -> UpdateVersionRowCommand -> Transaction ()
updateVersionRow versionId UpdateVersionRowCommand {..} =
  unless (null fieldAssignments) $ Sql.runBuilder D.noResult True sql
  where
    sql =
      "update news_versions set" <>
      Sql.csv fieldAssignments <>
      "where news_version_id =" <> Sql.param (getNewsVersionId versionId)
    fieldAssignments =
      catMaybes
        [ ("title =" <>) . Sql.param <$> uvrTitle
        , ("body =" <>) . Sql.param <$> uvrText
        , ("category_id =" <>) . Sql.param . fmap getCategoryId <$> uvrCategory
        , ("main_photo_id =" <>) . Sql.param . fmap getImageId <$> uvrMainPhoto
        ]

data UpdateVersionRowCommand =
  UpdateVersionRowCommand
    { uvrTitle :: Maybe T.Text
    , uvrText :: Maybe T.Text
    , uvrMainPhoto :: Maybe (Maybe ImageId)
    , uvrCategory :: Maybe (Maybe CategoryId)
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

setPhotosToVersion :: NewsVersionId -> Set.HashSet ImageId -> Transaction ()
setPhotosToVersion versionId photoIds
  -- stupid, but simple
 = do
  oldPhotoIds <- Set.fromList <$> unlinkAdditionalPhotosFromVersion versionId
  addPhotosToVersion versionId photoIds
  deleteImagesIfNotReferenced . toList $ photoIds `Set.difference` oldPhotoIds

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

setTagsToVersion :: NewsVersionId -> Set.HashSet TagId -> Transaction ()
setTagsToVersion versionId tagIds
  -- stupid, but simple
 = do
  unlinkTagsFromVersion versionId
  addTagsToVersion versionId tagIds

makeDraftIntoNews ::
     DraftId
  -> Day
  -> Transaction (Either IPublishDraft.MakeDraftIntoNewsFailure News)
makeDraftIntoNews draftId day =
  deleteDraftButLeaveItsContent draftId >>= \case
    Nothing -> pure $ Left IPublishDraft.MDNUnknownDraftId
    Just contentId -> do
      newsId <- insertNews contentId day
      getNews newsId >>= \case
        Nothing ->
          databaseInternalInconsistency $
          "Cannot find news just created: news_id=" <> showAsText newsId
        Just news -> pure $ Right news

overwriteNewsWithDraft ::
     NewsId
  -> DraftId
  -> Day
  -> Transaction (Either IPublishDraft.OverwriteNewsWithDraftFailure News)
overwriteNewsWithDraft newsId draftId day =
  deleteDraftButLeaveItsContent draftId >>= \case
    Nothing -> pure $ Left IPublishDraft.ONDUnknownDraftId
    Just contentId -> do
      oldContentId <-
        databaseUnsafeFromJust ("Cannot find news_id=" <> showAsText newsId) =<<
        getNewsContentId newsId
      setNewsContent contentId day newsId
      deleteNewsVersion oldContentId
      getNews newsId >>= \case
        Nothing ->
          databaseInternalInconsistency $
          "Cannot find news: news_id=" <> showAsText newsId
        Just news -> pure $ Right news

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

getNewsContentId :: NewsId -> Transaction (Maybe NewsVersionId)
getNewsContentId =
  runStatement $
  dimap
    getNewsId
    (fmap NewsVersionId)
    [TH.maybeStatement|
      select news_version_id :: integer
      from news
      where news_id = $1 :: integer
    |]

getDraftContentId :: DraftId -> Transaction (Maybe NewsVersionId)
getDraftContentId =
  runStatement $
  dimap
    getDraftId
    (fmap NewsVersionId)
    [TH.maybeStatement|
      select news_version_id :: integer
      from drafts
      where draft_id = $1 :: integer
    |]

-- | Updates the news with a new content and returns the old one's
-- identifier.
setNewsContent :: NewsVersionId -> Day -> NewsId -> Transaction ()
setNewsContent contentId day newsId =
  runStatement statement (contentId, day, newsId)
  where
    statement =
      lmap
        (\(contentId', day', newsId') ->
           (getNewsVersionId contentId', day', getNewsId newsId'))
        [TH.resultlessStatement|
          update news
          set news_version_id = $1 :: integer,
              "date" = $2 :: date
          where news_id = $3 :: integer
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
