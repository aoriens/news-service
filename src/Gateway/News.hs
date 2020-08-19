{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Gateway.News
  ( getNews
  ) where

import qualified Core.Interactor.GetNews as GetNews
import Core.Pagination
import Data.Foldable
import Data.Profunctor
import Data.Vector (Vector)
import qualified Database as DB
import qualified Hasql.Statement as Statement
import qualified Hasql.TH as TH

getNews :: DB.Handle -> Page -> IO [GetNews.News]
getNews h page = toList <$> DB.runStatement h selectNewsStatement page

selectNewsStatement :: Statement.Statement Page (Vector GetNews.News)
selectNewsStatement =
  dimap
    (\Page {..} -> (getPageLimit pageLimit, getPageOffset pageOffset))
    (fmap $ \(newsId, newsTitle, newsDate, newsText) -> GetNews.News {..})
    [TH.vectorStatement|
    select id :: integer, title :: varchar, date :: date, body :: varchar
    from news
    order by date desc, id desc
    limit $1 :: integer offset $2 :: integer
  |]
