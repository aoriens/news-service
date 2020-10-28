{-# LANGUAGE QuasiQuotes #-}

module Database.News
  ( getNews
  ) where

import Core.News
import Core.Pagination
import Data.Foldable
import Data.Profunctor
import Database
import qualified Hasql.TH as TH

getNews :: PageSpec -> Transaction [News]
getNews =
  statement $
  dimap
    (\PageSpec {..} -> (getPageLimit pageLimit, getPageOffset pageOffset))
    (map
       (\(rawNewsId, nvTitle, newsDate, nvText, rawNvId) ->
          News
            { newsId = NewsId rawNewsId
            , newsDate
            , newsVersion =
                NewsVersion {nvId = NewsVersionId rawNvId, nvTitle, nvText}
            }) .
     toList)
    [TH.vectorStatement|
    select news_id :: integer, title :: varchar, date :: date, body :: varchar, news_version_id :: integer
    from news join news_versions using (news_version_id)
    order by date desc, news_id desc
    limit $1 :: integer offset $2 :: integer
  |]
