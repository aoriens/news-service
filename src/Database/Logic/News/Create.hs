{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.Create
  ( createDraft
  , createNews
  , copyDraftFromNews
  ) where

import Control.Arrow
import Control.Monad.Catch
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
import Data.Coerce
import Data.Foldable
import qualified Data.HashSet as Set
import Data.Profunctor
import qualified Data.Text as T
import Data.Time
import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.Images
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
    nvId <-
      lift . insertVersion $
      InsertVersionCommand
        { ivcTitle = cdcTitle
        , ivcText = cdcText
        , ivcAuthorId = cdcAuthorId
        , ivcCategoryId = cdcCategoryId
        , ivcMainPhotoId = nvMainPhotoId
        }
    nvAdditionalPhotoIds <- createOrGetExistingAdditionalPhotos
    lift $ addPhotosToVersion nvId nvAdditionalPhotoIds
    lift $ addTagsToVersion nvId cdcTagIds
    pure
      Draft
        { draftId = coerce nvId
        , draftContent =
            NewsVersion
              { nvId
              , nvTitle = cdcTitle
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

insertVersion :: InsertVersionCommand -> Transaction NewsVersionId
insertVersion =
  runStatement $
  dimap
    (\InsertVersionCommand {..} ->
       ( ivcTitle
       , ivcText
       , getAuthorId ivcAuthorId
       , getCategoryId <$> ivcCategoryId
       , getImageId <$> ivcMainPhotoId))
    NewsVersionId
    [TH.singletonStatement|
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
      ) returning news_version_id :: integer
    |]

data InsertVersionCommand =
  InsertVersionCommand
    { ivcTitle :: T.Text
    , ivcText :: T.Text
    , ivcAuthorId :: AuthorId
    , ivcCategoryId :: Maybe CategoryId
    , ivcMainPhotoId :: Maybe ImageId
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

createNews :: NewsVersionId -> Day -> Transaction News
createNews vId day = do
  newsId' <- insertNews vId day
  getNews newsId' >>=
    maybe
      (throwM . DatabaseInternalInconsistencyException $
       "Cannot find news just created: news_id=" <> T.pack (show newsId'))
      pure

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

copyDraftFromNews :: NewsId -> Transaction NewsVersion
copyDraftFromNews newsId = do
  draftId <- copyDraftRowFromNews newsId
  copyAdditionalImagesFromNewsToVersion newsId draftId
  copyTagsFromNewsToVersion newsId draftId
  getDraft draftId >>= maybe draftNotFound pure
  where
    draftNotFound =
      throwM $
      DatabaseInternalInconsistencyException
        "Cannot find a just inserted draft row"

copyDraftRowFromNews :: NewsId -> Transaction NewsVersionId
copyDraftRowFromNews =
  runStatement $
  dimap
    getNewsId
    NewsVersionId
    [TH.singletonStatement|
      insert into news_versions
        (created_from_news_id, title, body, author_id, category_id, main_photo_id)
      select $1 :: integer, title, body, author_id, category_id, main_photo_id
      from news_versions
           join news using (news_version_id)
      where news_id = $1 :: integer
      returning news_version_id :: integer
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
