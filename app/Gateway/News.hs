module Gateway.News
  ( getNews
  ) where

import Data.Time.Calendar
import qualified Interactor.GetNews as GetNews

getNews :: IO [GetNews.News]
getNews =
  pure
    [ GetNews.News
        { GetNews.newsTitle = "Title1"
        , GetNews.newsDate = fromGregorian 2020 07 27
        , GetNews.newsText = "A news text"
        }
    , GetNews.News
        { GetNews.newsTitle = "Title2"
        , GetNews.newsDate = fromGregorian 2020 01 01
        , GetNews.newsText = "A news text"
        }
    ]
