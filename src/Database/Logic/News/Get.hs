{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.News.Get
  ( getNewsList
  , getNews
  ) where

import Control.Monad
import Core.Author
import Core.Category
import Core.Image
import qualified Core.Interactor.GetNews as IGetNews
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
import Database.Logic.News.Filter
import Database.Logic.Pagination
import Database.Logic.Tags
import Database.Service.Columns
import Database.Service.Primitives
import qualified Database.Service.SQLBuilder as Sql
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.TH as TH

getNewsList :: IGetNews.GatewayFilter -> PageSpec -> Transaction [News]
getNewsList newsFilter = mapM loadNewsWithRow <=< selectNewsRows newsFilter

getNews :: NewsId -> Transaction (Maybe News)
getNews = mapM loadNewsWithRow <=< selectNewsRow

selectNewsRows :: IGetNews.GatewayFilter -> PageSpec -> Transaction [NewsRow]
selectNewsRows f pageSpec =
  runStatementWithColumns sql newsRowColumns (fmap toList . D.rowVector) True
  where
    sql =
      topClause <>
      whereClauseToFilterNews f <> orderByClause <> limitOffsetClause
    topClause =
      Sql.text
        [TH.uncheckedSql|
          select distinct $COLUMNS
          from news
               join news_versions using (news_version_id)
               join authors using (author_id)
               join extended_users as users using (user_id)
               left join news_versions_and_tags_relation using (news_version_id)
               left join tags using (tag_id)
        |]
    orderByClause = "order by date desc, news_id desc"
    limitOffsetClause = pageSpecToLimitOffset pageSpec

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
