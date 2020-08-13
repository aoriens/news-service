{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Gateway.News
  ( getNews
  , Handle(..)
  ) where

import qualified Core.Interactor.GetNews as GetNews
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

getNews :: Handle -> IO [GetNews.News]
getNews Handle {..} =
  toList <$>
  DB.runStatement hWithConnection hLoggerHandle () selectNewsStatement

selectNewsStatement :: Statement.Statement () (Vector GetNews.News)
selectNewsStatement =
  rmap
    (fmap $ \(newsId, newsTitle, newsDate, newsText) -> GetNews.News {..})
    [TH.vectorStatement|
    select id :: integer, title :: varchar, date :: date, body :: varchar
    from news
  |]
