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

getNews :: Statement PageSpec [News]
getNews =
  dimap
    (\PageSpec {..} -> (getPageLimit pageLimit, getPageOffset pageOffset))
    (map
       (\(rawId, newsTitle, newsDate, newsText) ->
          News {newsId = NewsId rawId, ..}) .
     toList)
    [TH.vectorStatement|
    select news_id :: integer, title :: varchar, date :: date, body :: varchar
    from news join news_versions using (news_version_id)
    order by date desc, news_id desc
    limit $1 :: integer offset $2 :: integer
  |]
