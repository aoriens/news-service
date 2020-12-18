{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database.Logic.News.Get
  ( getNewsList
  , getNews
  , getDraftsOfAuthor
  , getDraftsOfUser
  , getDraft
  , getDraftIdsOfAuthor
  , getNewsAuthorId
  ) where

import Control.Monad
import Core.Author
import Core.Category
import Core.Deletable
import Core.Image
import qualified Core.Interactor.GetNewsList as IListNews
import Core.News
import Core.Pagination
import Core.Tag
import Core.User
import Data.Coerce
import Data.Foldable
import Data.Functor.Contravariant
import qualified Data.HashSet as Set
import Data.List
import Data.Profunctor
import qualified Data.Text as T
import Data.Time
import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.News.Filter
import Database.Logic.Pagination
import Database.Logic.Tags
import Database.Service.Columns
import Database.Service.Primitives
import qualified Database.Service.SQLBuilder as Sql
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.TH as TH

getNewsList ::
     IListNews.GatewayFilter
  -> IListNews.SortOptions
  -> PageSpec
  -> Transaction [News]
getNewsList filter' sortOptions =
  mapM loadNewsWithRow <=< selectNewsRows filter' sortOptions

getNews :: NewsId -> Transaction (Maybe News)
getNews = mapM loadNewsWithRow <=< selectNewsRow

selectNewsRows ::
     IListNews.GatewayFilter
  -> IListNews.SortOptions
  -> PageSpec
  -> Transaction [NewsRow]
selectNewsRows filter' sortOptions pageSpec =
  runStatementWithColumns sql newsRowColumns (fmap toList . D.rowVector) True
  where
    sql =
      topClause <>
      whereClauseToFilterNews filter' <>
      orderByClauseForListingNews sortOptions <> limitOffsetClause
    topClause =
      Sql.text
        [TH.uncheckedSql|
          select distinct $COLUMNS
          from news
               join news_versions using (news_version_id)
               left join authors using (author_id)
               left join extended_users as users using (user_id)
               left join news_versions_and_tags_relation using (news_version_id)
               left join tags using (tag_id)
        |]
    limitOffsetClause = limitOffsetClauseWithPageSpec pageSpec

orderByClauseForListingNews :: IListNews.SortOptions -> Sql.Builder
orderByClauseForListingNews IListNews.SortOptions {..} =
  ("order by" <>) .
  mconcat . intersperse "," . map (<> direction) . (++ ["news_id"]) $
  fields
  where
    direction
      | sortReverse = "desc"
      | otherwise = mempty
    fields =
      case sortKey of
        IListNews.SortKeyDate -> ["date"]
        IListNews.SortKeyAuthorName -> ["users.last_name", "users.first_name"]

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
             left join authors using (author_id)
             left join users using (user_id)
        where news_id = $1
      |]

getDraftsOfAuthor :: AuthorId -> PageSpec -> Transaction [Draft]
getDraftsOfAuthor authorId pageSpec =
  mapM loadDraftWithRow =<< getDraftRowsOfAuthor authorId pageSpec

getDraftsOfUser :: UserId -> PageSpec -> Transaction [Draft]
getDraftsOfUser userId pageSpec =
  mapM loadDraftWithRow =<< getDraftRowsOfUser userId pageSpec

getDraft :: DraftId -> Transaction (Maybe Draft)
getDraft = mapM loadDraftWithRow <=< getDraftRow

getDraftRowsOfAuthor :: AuthorId -> PageSpec -> Transaction [VersionRow]
getDraftRowsOfAuthor authorId pageSpec =
  runStatementWithColumns sql versionRowColumns D.rowList True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
          select $COLUMNS
          from drafts as news_versions
               left join authors using (author_id)
               left join users using (user_id)
          where news_versions.author_id =
        |] <>
      Sql.param (getAuthorId authorId) <> limitOffsetClauseWithPageSpec pageSpec

getDraftRowsOfUser :: UserId -> PageSpec -> Transaction [VersionRow]
getDraftRowsOfUser userId pageSpec =
  runStatementWithColumns sql versionRowColumns D.rowList True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
          select $COLUMNS
          from drafts as news_versions
               left join authors using (author_id)
               left join users using (user_id)
          where authors.user_id =
        |] <>
      Sql.param (getUserId userId) <> limitOffsetClauseWithPageSpec pageSpec

getDraftIdsOfAuthor :: AuthorId -> Transaction [NewsVersionId]
getDraftIdsOfAuthor =
  runStatement $
  dimap
    getAuthorId
    (toList . fmap NewsVersionId)
    [TH.vectorStatement|
      select news_version_id :: integer
      from drafts
      where author_id = $1 :: integer
    |]

getDraftRow :: DraftId -> Transaction (Maybe VersionRow)
getDraftRow draftId =
  runStatementWithColumns sql versionRowColumns D.rowMaybe True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
          select $COLUMNS
          from drafts as news_versions
               left join authors using (author_id)
               left join users using (user_id)
          where news_version_id =
        |] <>
      Sql.param (getDraftId draftId)

-- Part of news we can extract from the database with the first query.
data NewsRow =
  NewsRow
    { newsId :: NewsId
    , newsDate :: Day
    , newsContentRow :: VersionRow
    }

newsRowColumns :: Columns NewsRow
newsRowColumns = do
  newsId <- NewsId <$> column newsTable "news_id"
  newsDate <- column newsTable "date"
  newsContentRow <- versionRowColumns
  pure NewsRow {..}

newsTable :: TableName
newsTable = "news"

-- Part of news version we can extract from the database with the first query.
data VersionRow =
  VersionRow
    { nvId :: NewsVersionId
    , nvTitle :: T.Text
    , nvText :: T.Text
    , nvAuthor :: Deletable Author
    , nvCategoryId :: Maybe CategoryId
    , nvMainPhotoId :: Maybe ImageId
    }

versionRowColumns :: Columns VersionRow
versionRowColumns = do
  nvId <- NewsVersionId <$> column versionsTable "news_version_id"
  nvTitle <- column versionsTable "title"
  nvText <- column versionsTable "body"
  nvAuthor <- deletableAuthorColumns
  nvCategoryId <- fmap CategoryId <$> column versionsTable "category_id"
  nvMainPhotoId <- fmap ImageId <$> column versionsTable "main_photo_id"
  pure VersionRow {..}

versionsTable :: TableName
versionsTable = "news_versions"

loadNewsWithRow :: NewsRow -> Transaction News
loadNewsWithRow NewsRow {..} = do
  newsContent <- loadVersionWithRow newsContentRow
  pure News {newsContent, ..}

loadDraftWithRow :: VersionRow -> Transaction Draft
loadDraftWithRow = fmap makeDraft . loadVersionWithRow
  where
    makeDraft version@NewsVersion {nvId} =
      Draft {draftId = coerce nvId, draftContent = version}

loadVersionWithRow :: VersionRow -> Transaction NewsVersion
loadVersionWithRow VersionRow {..} = do
  nvCategory <-
    maybe (pure Nothing) (fmap Just . getExistingCategoryById) nvCategoryId
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

getNewsAuthorId :: NewsId -> Transaction (Maybe (Deletable AuthorId))
getNewsAuthorId =
  runStatement $
  dimap
    getNewsId
    (fmap (deletableFromMaybe . fmap AuthorId))
    [TH.maybeStatement|
      select author_id :: integer?
      from news
           join news_versions using (news_version_id)
      where news_id = $1 :: integer
    |]
