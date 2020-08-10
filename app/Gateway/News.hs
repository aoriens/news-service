{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Gateway.News
  ( getNews
  , Handle(..)
  ) where

import Data.Foldable
import Data.Profunctor
import Data.Vector (Vector)
import qualified Database as DB
import qualified Hasql.Connection as C
import qualified Hasql.Statement as Statement
import qualified Hasql.TH as TH
import qualified Interactor.GetNews as GetNews

newtype Handle =
  Handle
    { hWithConnection :: forall a. (C.Connection -> IO a) -> IO a
    }

getNews :: Handle -> IO [GetNews.News]
getNews h =
  toList <$> DB.runStatement (hWithConnection h) () selectNewsStatement

selectNewsStatement :: Statement.Statement () (Vector GetNews.News)
selectNewsStatement =
  rmap
    (fmap toNews)
    [TH.vectorStatement|
    select title :: text, date :: date, body :: text
    from news
  |]
  where
    toNews (title, date, body) =
      GetNews.News
        { GetNews.newsTitle = title
        , GetNews.newsDate = date
        , GetNews.newsText = body
        }
