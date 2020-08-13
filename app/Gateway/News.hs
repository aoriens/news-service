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

getNews :: Handle -> PageLimit -> IO [GetNews.News]
getNews Handle {..} pageLimit =
  toList <$>
  DB.runStatement hWithConnection hLoggerHandle pageLimit selectNewsStatement

selectNewsStatement :: Statement.Statement PageLimit (Vector GetNews.News)
selectNewsStatement =
  dimap
    getPageLimit
    (fmap $ \(newsId, newsTitle, newsDate, newsText) -> GetNews.News {..})
    [TH.vectorStatement|
    select id :: integer, title :: varchar, date :: date, body :: varchar
    from news
    limit $1 :: integer
  |]
