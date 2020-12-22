{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database.Logic.News.Get
  ( getNewsList
  , getNews
  , getDraftsOfAuthor
  , getDraftsOfUser
  , getDraftsCreatedFromNewsId
  , getDraft
  , getDraftIdsOfAuthor
  , getNewsAuthorId
  , getDraftAuthor
  , getDraftAuthorAndNewsIdItWasCreatedFrom
  ) where

import Control.Arrow
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
import Data.Foldable
import Data.Functor.Contravariant
import qualified Data.HashSet as Set
import Data.Profunctor
import qualified Data.Text as T
import Data.Text.Show
import Data.Time
import Database.Logic.Authors
import Database.Logic.Categories
import Database.Logic.News.Filter
import Database.Logic.News.NewsVersionId
import Database.Logic.Pagination
import Database.Logic.Tags
import Database.Service.Columns
import Database.Service.Primitives
import qualified Database.Service.SQLBuilder as Sql
import qualified Database.Service.SQLBuilders as Sql
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
          with recursive num_photos_for_news_versions as (
            select news_version_id,
                   count (image_id) + case when main_photo_id is null
                                           then 0 else 1 end
                     as num_photos
            from news_versions
                 left join news_versions_and_additional_photos_relation using (news_version_id)
            group by news_version_id
          ), category_full_names_intermediate as (
            select category_id, parent_id, array[name] as full_name
            from categories

            union

            select children.category_id, parents.parent_id, parents.name || children.full_name
            from category_full_names_intermediate as children
                 join categories as parents on children.parent_id = parents.category_id
          ), category_full_names as (
            select category_id, full_name
            from category_full_names_intermediate
            where parent_id is null
          )
          select distinct $COLUMNS,
                 -- ordering columns only
                 num_photos, category_full_names.full_name
          from news
               join news_versions using (news_version_id)
               left join authors using (author_id)
               left join extended_users as users using (user_id)
               left join news_versions_and_tags_relation using (news_version_id)
               left join tags using (tag_id)
               join num_photos_for_news_versions using (news_version_id)
               left join category_full_names using (category_id)
        |]
    limitOffsetClause = limitOffsetClauseWithPageSpec pageSpec

orderByClauseForListingNews :: IListNews.SortOptions -> Sql.Builder
orderByClauseForListingNews IListNews.SortOptions {..} =
  ("order by" <>) . Sql.csv . map (<> direction) $ fields ++ ["news_id"]
  where
    direction
      | sortReverse = "desc"
      | otherwise = mempty
    fields =
      case sortKey of
        IListNews.SortKeyDate -> ["date"]
        IListNews.SortKeyAuthorName -> ["users.last_name", "users.first_name"]
        IListNews.SortKeyNumPhotos -> ["num_photos"]
        IListNews.SortKeyCategoryName -> ["category_full_names.full_name"]

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

getDraftsCreatedFromNewsId :: NewsId -> PageSpec -> Transaction [Draft]
getDraftsCreatedFromNewsId newsId pageSpec =
  mapM loadDraftWithRow =<< getRowsOfDraftsCreatedFromNews newsId pageSpec

getDraft :: DraftId -> Transaction (Maybe Draft)
getDraft = mapM loadDraftWithRow <=< getDraftRow

getDraftRowsOfAuthor :: AuthorId -> PageSpec -> Transaction [DraftRow]
getDraftRowsOfAuthor authorId pageSpec =
  runStatementWithColumns sql draftRowColumns D.rowList True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
          select $COLUMNS
          from drafts
               join news_versions using (news_version_id)
               left join authors using (author_id)
               left join users using (user_id)
          where news_versions.author_id =
        |] <>
      Sql.param (getAuthorId authorId) <> limitOffsetClauseWithPageSpec pageSpec

getDraftRowsOfUser :: UserId -> PageSpec -> Transaction [DraftRow]
getDraftRowsOfUser userId pageSpec =
  runStatementWithColumns sql draftRowColumns D.rowList True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
          select $COLUMNS
          from drafts
               join news_versions using (news_version_id)
               left join authors using (author_id)
               left join users using (user_id)
          where authors.user_id =
        |] <>
      Sql.param (getUserId userId) <> limitOffsetClauseWithPageSpec pageSpec

getRowsOfDraftsCreatedFromNews :: NewsId -> PageSpec -> Transaction [DraftRow]
getRowsOfDraftsCreatedFromNews newsId pageSpec =
  runStatementWithColumns sql draftRowColumns D.rowList True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
          select $COLUMNS
          from drafts
               join news_versions using (news_version_id)
               left join authors using (author_id)
               left join users using (user_id)
          where drafts.created_from_news_id =
        |] <>
      Sql.param (getNewsId newsId) <> limitOffsetClauseWithPageSpec pageSpec

getDraftIdsOfAuthor :: AuthorId -> Transaction [DraftId]
getDraftIdsOfAuthor =
  runStatement $
  dimap
    getAuthorId
    (toList . fmap DraftId)
    [TH.vectorStatement|
      select draft_id :: integer
      from drafts
           join news_versions using (news_version_id)
      where author_id = $1 :: integer
    |]

getDraftRow :: DraftId -> Transaction (Maybe DraftRow)
getDraftRow draftId =
  runStatementWithColumns sql draftRowColumns D.rowMaybe True
  where
    sql =
      Sql.text
        [TH.uncheckedSql|
          select $COLUMNS
          from drafts
               join news_versions using (news_version_id)
               left join authors using (author_id)
               left join users using (user_id)
          where draft_id =
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

data DraftRow =
  DraftRow
    { draftId :: DraftId
    , draftContentRow :: VersionRow
    , draftNewsIdItWasCreatedFrom :: Maybe NewsId
    }

draftRowColumns :: Columns DraftRow
draftRowColumns = do
  draftId <- DraftId <$> column draftTable "draft_id"
  draftNewsIdItWasCreatedFrom <-
    fmap NewsId <$> column draftTable "created_from_news_id"
  draftContentRow <- versionRowColumns
  pure DraftRow {..}

draftTable :: TableName
draftTable = "drafts"

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

loadDraftWithRow :: DraftRow -> Transaction Draft
loadDraftWithRow DraftRow {..} = do
  draftContent <- loadVersionWithRow draftContentRow
  pure Draft {draftContent, ..}

loadVersionWithRow :: VersionRow -> Transaction NewsVersion
loadVersionWithRow VersionRow {..} = do
  nvCategory <-
    maybe (pure Nothing) (fmap Just . getExistingCategoryById) nvCategoryId
  nvTags <- getTagsForVersion nvId
  nvAdditionalPhotoIds <- getAdditionalPhotosForVersion nvId
  pure NewsVersion {nvCategory, nvTags, nvAdditionalPhotoIds, ..}

getExistingCategoryById :: CategoryId -> Transaction Category
getExistingCategoryById catId =
  databaseUnsafeFromJust
    ("Suddenly not found: category_id=" <> showAsText catId) =<<
  selectCategory catId

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

getDraftAuthor :: DraftId -> Transaction (Maybe (Deletable AuthorId))
getDraftAuthor =
  runStatement $
  dimap
    getDraftId
    (fmap (deletableFromMaybe . fmap AuthorId))
    [TH.maybeStatement|
      select author_id :: integer?
      from drafts
           join news_versions using (news_version_id)
      where draft_id = $1 :: integer
    |]

getDraftAuthorAndNewsIdItWasCreatedFrom ::
     DraftId -> Transaction (Maybe (Deletable AuthorId, Maybe NewsId))
getDraftAuthorAndNewsIdItWasCreatedFrom =
  runStatement $
  dimap
    getDraftId
    (fmap (deletableFromMaybe . fmap AuthorId *** fmap NewsId))
    [TH.maybeStatement|
      select author_id :: integer?, created_from_news_id :: integer?
      from drafts
           join news_versions using (news_version_id)
      where draft_id = $1 :: integer
    |]
