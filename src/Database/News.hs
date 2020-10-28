{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.News
  ( getNews
  ) where

import Control.Monad
import Core.Author
import Core.Category
import Core.Image
import Core.News
import Core.Pagination
import Data.Foldable
import Data.Text (Text)
import Data.Time
import Database
import Database.Authors
import Database.Categories
import Database.Columns
import Database.Pagination
import qualified Hasql.Decoders as D
import qualified Hasql.TH as TH

getNews :: PageSpec -> Transaction [News]
getNews = mapM loadNewsWithPartialNews <=< selectPartialNews

selectPartialNews :: PageSpec -> Transaction [PartialNews]
selectPartialNews =
  statement $
  statementWithColumns
    sql
    pageToLimitOffsetEncoder
    partialNewsColumns
    (fmap toList . D.rowVector)
    True
  where
    sql =
      [TH.uncheckedSql|
        select $COLUMNS
        from news join news_versions using (news_version_id)
                  join authors using (author_id)
                  join users using (user_id)
        order by date desc, news_id desc
        limit $1 offset $2
      |]

-- Part of news we can extract from the database with the first query.
data PartialNews =
  PartialNews
    { newsId :: NewsId
    , newsDate :: Day
    , newsPartialVersion :: PartialVersion
    }

partialNewsColumns :: Columns PartialNews
partialNewsColumns = do
  newsId <- NewsId <$> column newsTable "news_id"
  newsDate <- column newsTable "date"
  newsPartialVersion <- partialVersionColumns
  pure PartialNews {..}

newsTable :: TableName
newsTable = "news"

-- Part of news version we can extract from the database with the first query.
data PartialVersion =
  PartialVersion
    { nvId :: NewsVersionId
    , nvTitle :: Text
    , nvText :: Text
    , nvAuthor :: Author
    , nvCategoryId :: CategoryId
    , nvMainPhotoId :: Maybe ImageId
    }

partialVersionColumns :: Columns PartialVersion
partialVersionColumns = do
  nvId <- NewsVersionId <$> column versionsTable "news_version_id"
  nvTitle <- column versionsTable "title"
  nvText <- column versionsTable "body"
  nvAuthor <- authorColumns
  nvCategoryId <- CategoryId <$> column versionsTable "category_id"
  nvMainPhotoId <- fmap ImageId <$> column versionsTable "main_photo_id"
  pure PartialVersion {..}

versionsTable :: TableName
versionsTable = "news_versions"

loadNewsWithPartialNews :: PartialNews -> Transaction News
loadNewsWithPartialNews PartialNews {..} = do
  newsVersion <- loadVersionWithPartialVersion newsPartialVersion
  pure News {newsVersion, ..}

loadVersionWithPartialVersion :: PartialVersion -> Transaction NewsVersion
loadVersionWithPartialVersion PartialVersion {..} = do
  nvCategory <-
    maybe
      (databaseInternalInconsistency
         "NewsVersion must always refer to an existing category")
      pure =<<
    selectCategory nvCategoryId
  pure NewsVersion {nvCategory, ..}
