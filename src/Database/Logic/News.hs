{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.News
  ( getNewsList
  , createNewsVersion
  , getDraftAuthor
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
import qualified Core.Interactor.GetNews as IGetNews
import qualified Core.Interactor.PublishDraft as IPublishDraft
import Core.News
import Core.Pagination
import Core.Tag
import Data.Foldable
import Data.Functor.Contravariant
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as N
import Data.Profunctor
import qualified Data.Text as T
import Data.Time
import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.Images
import Database.Logic.Tags
import Database.Service.Columns
import Database.Service.Primitives
import Database.Service.SQLBuilder
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.TH as TH

getNewsList :: IGetNews.GatewayNewsFilter -> PageSpec -> Transaction [News]
getNewsList newsFilter = mapM loadNewsWithRow <=< selectNewsRows newsFilter

getNews :: NewsId -> Transaction (Maybe News)
getNews = mapM loadNewsWithRow <=< selectNewsRow

selectNewsRows ::
     IGetNews.GatewayNewsFilter -> PageSpec -> Transaction [NewsRow]
selectNewsRows IGetNews.GatewayNewsFilter {..} pageSpec =
  runStatementWithColumns sql newsRowColumns (fmap toList . D.rowVector) True
  where
    sql = topClause <> whereClause <> orderByClause <> limitOffsetClause
    topClause =
      sqlText
        [TH.uncheckedSql|
          select $COLUMNS
          from news
               join news_versions using (news_version_id)
               join authors using (author_id)
               join users using (user_id)
        |]
    whereClause =
      ("where" <>) $
      dateCondition gnfDateRanges `sqlAnd` authorCondition gnfAuthorFilter `ifSQLBuilderEmpty`
      "true"
    orderByClause = "order by date desc, news_id desc"
    limitOffsetClause = sqlLimitOffset pageSpec

dateCondition :: Maybe (N.NonEmpty IGetNews.NewsDateRange) -> SQLBuilder
dateCondition =
  maybe mempty $ foldr (sqlOr . sqlWithinDateRange "\"date\"") mempty

authorCondition :: IGetNews.GatewayNewsAuthorFilter -> SQLBuilder
authorCondition IGetNews.GatewayNewsAuthorFilter {..} =
  idCondition `sqlOr` nameCondition
  where
    idCondition =
      case gnfAuthorIds of
        Nothing -> mempty
        Just ids ->
          "authors.author_id =" <>
          sqlAny (sqlParam . map getAuthorId $ toList ids)
    nameCondition =
      case gnfAuthorNames of
        Nothing -> mempty
        Just names ->
          fullName <>
          "ilike" <> sqlAny (sqlParam $ map patternFromName $ toList names)
    fullName =
      "coalesce(users.first_name || ' ' || users.last_name, users.last_name)"
    patternFromName = T.cons '%' . (`T.snoc` '%') . sqlEscapeLikePattern

sqlEscapeLikePattern :: T.Text -> T.Text
sqlEscapeLikePattern = T.concatMap f
  where
    f char
      | shouldEscape char = escapeChar `T.cons` T.singleton char
      | otherwise = T.singleton char
    shouldEscape char = char == escapeChar || char == '%' || char == '_'
    escapeChar = '\\'

sqlAny :: SQLBuilder -> SQLBuilder
sqlAny e = "any (" <> e <> ")"

sqlOr :: SQLBuilder -> SQLBuilder -> SQLBuilder
sqlOr = sqlBinaryOperationIfNonEmpty "or"

sqlAnd :: SQLBuilder -> SQLBuilder -> SQLBuilder
sqlAnd = sqlBinaryOperationIfNonEmpty "and"

sqlBinaryOperationIfNonEmpty ::
     SQLBuilder -> SQLBuilder -> SQLBuilder -> SQLBuilder
sqlBinaryOperationIfNonEmpty op x y
  | sqlBuilderIsEmpty x = y
  | sqlBuilderIsEmpty y = x
  | otherwise = x <> op <> y

sqlWithinDateRange :: SQLBuilder -> IGetNews.NewsDateRange -> SQLBuilder
sqlWithinDateRange expr dateRange =
  case dateRange of
    IGetNews.NewsSinceUntil from to
      | from == to -> sqlEqual expr $ sqlParam from
      | otherwise -> sqlBetween expr (sqlParam from, sqlParam to)
    IGetNews.NewsSince day -> sqlGreaterOrEqual expr $ sqlParam day
    IGetNews.NewsUntil day -> sqlLessOrEqual expr $ sqlParam day

sqlBetween :: SQLBuilder -> (SQLBuilder, SQLBuilder) -> SQLBuilder
sqlBetween expr (from, to) = expr <> "between" <> from <> "and" <> to

sqlEqual :: SQLBuilder -> SQLBuilder -> SQLBuilder
sqlEqual x y = x <> "=" <> y

sqlGreaterOrEqual :: SQLBuilder -> SQLBuilder -> SQLBuilder
sqlGreaterOrEqual x y = x <> ">=" <> y

sqlLessOrEqual :: SQLBuilder -> SQLBuilder -> SQLBuilder
sqlLessOrEqual x y = x <> "<=" <> y

sqlLimitOffset :: PageSpec -> SQLBuilder
sqlLimitOffset PageSpec {..} =
  "limit" <>
  sqlParam (getPageLimit pageLimit) <>
  "offset" <> sqlParam (getPageOffset pageOffset)

selectNewsRow :: NewsId -> Transaction (Maybe NewsRow)
selectNewsRow =
  runStatement $
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
  runStatement $
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
  runStatement $
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

getDraftAuthor ::
     NewsVersionId -> Transaction (Either IPublishDraft.GatewayFailure AuthorId)
getDraftAuthor =
  runStatement $
  dimap
    getNewsVersionId
    (maybe (Left IPublishDraft.UnknownDraftId) (Right . AuthorId))
    [TH.maybeStatement|
      select author_id :: integer
      from drafts
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
