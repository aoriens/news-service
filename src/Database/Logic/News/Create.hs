{-# LANGUAGE QuasiQuotes #-}

module Database.Logic.News.Create
  ( createNewsVersion
  , createNews
  ) where

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Author
import Core.Category
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
import Database.Logic.News.Get
import Database.Logic.Tags
import Database.Service.Primitives
import qualified Hasql.TH as TH

createNewsVersion ::
     ICreateDraft.CreateNewsVersionCommand
  -> Transaction (Either ICreateDraft.GatewayFailure NewsVersion)
createNewsVersion ICreateDraft.CreateNewsVersionCommand {..} =
  runExceptT $ do
    nvAuthor <- getExistingEntityBy selectAuthorById cnvAuthorId
    nvCategory <- getExistingEntityBy selectCategory cnvCategoryId
    nvTags <- getExistingTags
    nvMainPhotoId <- mapM createOrGetExistingImage cnvMainPhoto
    nvId <- lift $ insertVersion' nvMainPhotoId
    nvAdditionalPhotoIds <- createOrGetExistingAdditionalPhotos
    lift $ addPhotosToVersion nvId nvAdditionalPhotoIds
    lift $ addTagsToVersion nvId cnvTagIds
    pure
      NewsVersion
        { nvId
        , nvTitle = cnvTitle
        , nvText = cnvText
        , nvAuthor
        , nvCategory
        , nvMainPhotoId
        , nvAdditionalPhotoIds
        , nvTags
        }
  where
    getExistingEntityBy getOptEntity id' = do
      optEntity <- lift $ getOptEntity id'
      case optEntity of
        Nothing -> failWithEntityNotFound id'
        Just entity -> pure entity
    getExistingTags =
      Set.fromList <$> mapM (getExistingEntityBy findTagById) (toList cnvTagIds)
    createOrGetExistingImage img
      | Right image <- img = lift $ createImage image
      | Left imageId' <- img = do
        exists <- lift $ imageExists imageId'
        if exists
          then pure imageId'
          else failWithEntityNotFound imageId'
    insertVersion' photoId =
      insertVersion
        InsertVersionCommand
          { ivcTitle = cnvTitle
          , ivcText = cnvText
          , ivcAuthorId = cnvAuthorId
          , ivcCategoryId = cnvCategoryId
          , ivcMainPhotoId = photoId
          }
    createOrGetExistingAdditionalPhotos =
      Set.fromList <$> mapM createOrGetExistingImage cnvAdditionalPhotos
    failWithEntityNotFound objId =
      throwE $ ICreateDraft.GUnknownEntityId [toEntityId objId]

insertVersion :: InsertVersionCommand -> Transaction NewsVersionId
insertVersion =
  runStatement $
  dimap
    (\InsertVersionCommand {..} ->
       ( ivcTitle
       , ivcText
       , getAuthorId ivcAuthorId
       , getCategoryId ivcCategoryId
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
        $4 :: integer,
        $5 :: integer?
      ) returning news_version_id :: integer
    |]

data InsertVersionCommand =
  InsertVersionCommand
    { ivcTitle :: T.Text
    , ivcText :: T.Text
    , ivcAuthorId :: AuthorId
    , ivcCategoryId :: CategoryId
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