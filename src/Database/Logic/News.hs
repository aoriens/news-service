{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.News
  ( getNewsList
  , createNewsVersion
  , getAuthorOfNewsVersion
  , createNews
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Author
import Core.Category
import Core.EntityId
import Core.Image
import qualified Core.Interactor.CreateDraft as ICreateDraft
import qualified Core.Interactor.PublishDraft as IPublishDraft
import Core.News
import Core.Pagination
import Core.Tag
import Data.Foldable
import Data.Functor.Contravariant
import qualified Data.HashSet as Set
import Data.Profunctor
import qualified Data.Text as T
import Data.Time
import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.Images
import Database.Logic.Pagination
import Database.Logic.Tags
import Database.Service.Columns
import Database.Service.Primitives
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.TH as TH

getNewsList :: PageSpec -> Transaction [News]
getNewsList = mapM loadNewsWithRow <=< selectNewsRows

getNews :: NewsId -> Transaction (Maybe News)
getNews = mapM loadNewsWithRow <=< selectNewsRow

selectNewsRows :: PageSpec -> Transaction [NewsRow]
selectNewsRows =
  statement $
  statementWithColumns
    sql
    pageToLimitOffsetEncoder
    newsRowColumns
    (fmap toList . D.rowVector)
    True
  where
    sql =
      [TH.uncheckedSql|
        select $COLUMNS
        from news
             join news_versions using (news_version_id)
             join authors using (author_id)
             join users using (user_id)
        order by date desc, news_id desc
        limit $1 offset $2
      |]

selectNewsRow :: NewsId -> Transaction (Maybe NewsRow)
selectNewsRow =
  statement $
  statementWithColumns
    sql
    (getNewsId >$< (E.param . E.nonNullable) E.int4)
    newsRowColumns
    D.rowMaybe
    True
  where
    sql =
      [TH.uncheckedSql|
        select $COLUMNS
        from news
             join news_versions using (news_version_id)
             join authors using (author_id)
             join users using (user_id)
        where news_id = $1
      |]

-- Part of news we can extract from the database with the first query.
data NewsRow =
  NewsRow
    { newsId :: NewsId
    , newsDate :: Day
    , newsVersionRow :: VersionRow
    }

newsRowColumns :: Columns NewsRow
newsRowColumns = do
  newsId <- NewsId <$> column newsTable "news_id"
  newsDate <- column newsTable "date"
  newsVersionRow <- versionRowColumns
  pure NewsRow {..}

newsTable :: TableName
newsTable = "news"

-- Part of news version we can extract from the database with the first query.
data VersionRow =
  VersionRow
    { nvId :: NewsVersionId
    , nvTitle :: T.Text
    , nvText :: T.Text
    , nvAuthor :: Author
    , nvCategoryId :: CategoryId
    , nvMainPhotoId :: Maybe ImageId
    }

versionRowColumns :: Columns VersionRow
versionRowColumns = do
  nvId <- NewsVersionId <$> column versionsTable "news_version_id"
  nvTitle <- column versionsTable "title"
  nvText <- column versionsTable "body"
  nvAuthor <- authorColumns
  nvCategoryId <- CategoryId <$> column versionsTable "category_id"
  nvMainPhotoId <- fmap ImageId <$> column versionsTable "main_photo_id"
  pure VersionRow {..}

versionsTable :: TableName
versionsTable = "news_versions"

loadNewsWithRow :: NewsRow -> Transaction News
loadNewsWithRow NewsRow {..} = do
  newsVersion <- loadVersionWithRow newsVersionRow
  pure News {newsVersion, ..}

loadVersionWithRow :: VersionRow -> Transaction NewsVersion
loadVersionWithRow VersionRow {..} = do
  nvCategory <- getExistingCategoryById nvCategoryId
  nvTags <- getTagsForVersion nvId
  nvAdditionalPhotoIds <- getAdditionalPhotosForVersion nvId
  pure NewsVersion {nvCategory, nvTags, nvAdditionalPhotoIds, ..}

getExistingCategoryById :: CategoryId -> Transaction Category
getExistingCategoryById =
  maybe
    (databaseInternalInconsistency
       "NewsVersion must always refer to an existing category")
    pure <=<
  selectCategory

getTagsForVersion :: NewsVersionId -> Transaction (Set.HashSet Tag)
getTagsForVersion =
  statement $
  statementWithColumns
    sql
    encoder
    tagColumns
    (fmap (Set.fromList . toList) . D.rowVector)
    True
  where
    sql =
      [TH.uncheckedSql|
        select $COLUMNS
        from news_versions_and_tags_relation
             join tags using (tag_id)
        where news_version_id = $1
      |]
    encoder = getNewsVersionId >$< (E.param . E.nonNullable) E.int4

getAdditionalPhotosForVersion ::
     NewsVersionId -> Transaction (Set.HashSet ImageId)
getAdditionalPhotosForVersion =
  statement $
  dimap
    getNewsVersionId
    (Set.fromList . map ImageId . toList)
    [TH.vectorStatement|
      select image_id :: integer
      from news_versions_and_additional_photos_relation
      where news_version_id = $1 :: integer
    |]

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
  statement $
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
  curry . statement $
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
  curry . statement $
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

getAuthorOfNewsVersion ::
     NewsVersionId -> Transaction (Either IPublishDraft.GatewayFailure AuthorId)
getAuthorOfNewsVersion =
  statement $
  dimap
    getNewsVersionId
    (maybe (Left IPublishDraft.UnknownNewsVersionId) (Right . AuthorId))
    [TH.maybeStatement|
      select author_id :: integer
      from news_versions
      where news_version_id = $1 :: integer
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
  curry . statement $
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
