{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Gateway.News
  ( getNews
  , Handle(..)
  ) where

import Data.Foldable
import Data.Profunctor
import Data.Vector (Vector)
import qualified Hasql.Connection as C
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.TH as TH
import qualified Interactor.GetNews as GetNews

newtype Handle =
  Handle
    { hWithConnection :: forall a. (C.Connection -> IO a) -> IO a
    }

getNews :: Handle -> IO [GetNews.News]
getNews h = do
  r <- runStatement h () selectNewsStatement
  either (error . show) (pure . toList) r

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

runSession :: Handle -> Session.Session a -> IO (Either Session.QueryError a)
runSession h = hWithConnection h . Session.run

runStatement ::
     Handle
  -> params
  -> Statement.Statement params result
  -> IO (Either Session.QueryError result)
runStatement h params = runSession h . Session.statement params
