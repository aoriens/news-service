{-# LANGUAGE QuasiQuotes #-}

module Gateway.News
  ( getNews
  ) where

import Control.Exception
import Data.Foldable
import Data.Profunctor
import Data.Vector (Vector)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.TH as TH
import qualified Interactor.GetNews as GetNews

getNews :: IO [GetNews.News]
getNews = do
  r <- runStatement () selectNewsStatement
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

withConnection :: (Connection.Connection -> IO a) -> IO a
withConnection = bracket getConnection Connection.release
  where
    getConnection = do
      r <- Connection.acquire "dbname=news"
      case r of
        Left e -> error ("Hasql error: " ++ show e)
        Right conn -> pure conn

runSession :: Session.Session a -> IO (Either Session.QueryError a)
runSession = withConnection . Session.run

runStatement ::
     params
  -> Statement.Statement params result
  -> IO (Either Session.QueryError result)
runStatement params = runSession . Session.statement params
