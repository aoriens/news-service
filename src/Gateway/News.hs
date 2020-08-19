{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Gateway.News
  ( getNews
  , Handle(..)
  ) where

import qualified Core.Interactor.GetNews as GetNews
import Core.Pagination
import Data.Foldable
import Data.Profunctor
import Data.Vector (Vector)
import qualified Database as DB
import qualified Hasql.Connection as C
import qualified Hasql.Statement as Statement
import qualified Hasql.TH as TH
import qualified Logger

data Handle =
  Handle
    { hWithConnection :: forall a. (C.Connection -> IO a) -> IO a
    , hLoggerHandle :: Logger.Handle IO
    }

getNews :: Handle -> Page -> IO [GetNews.News]
getNews Handle {..} page =
  toList <$>
  DB.runStatement hWithConnection hLoggerHandle selectNewsStatement page

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
