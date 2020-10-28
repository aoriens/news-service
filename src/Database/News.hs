{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.News
  ( getNews
  ) where

import Core.News
import Core.Pagination
import Data.Foldable
import Database
import Database.Columns
import Database.Pagination
import qualified Hasql.Decoders as D
import qualified Hasql.TH as TH

getNews :: PageSpec -> Transaction [News]
getNews =
  statement $
  statementWithColumns
    sql
    pageToLimitOffsetEncoder
    newsColumns
    (fmap toList . D.rowVector)
    True
  where
    sql =
      [TH.uncheckedSql|
        select $COLUMNS
        from news join news_versions using (news_version_id)
        order by date desc, news_id desc
        limit $1 offset $2
      |]

newsColumns :: Columns News
newsColumns = do
  newsId <- NewsId <$> column newsTable "news_id"
  newsDate <- column newsTable "date"
  newsVersion <- versionColumns
  pure News {..}

newsTable :: TableName
newsTable = "news"

versionColumns :: Columns NewsVersion
versionColumns = do
  nvId <- NewsVersionId <$> column versionsTable "news_version_id"
  nvTitle <- column versionsTable "title"
  nvText <- column versionsTable "body"
  pure NewsVersion {..}

versionsTable :: TableName
versionsTable = "news_versions"
