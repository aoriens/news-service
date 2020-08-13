module Core.Interactor.GetNews
  ( Handle(..)
  , getNews
  , News(..)
  ) where

import Data.Int
import Data.Text (Text)
import Data.Time.Calendar

newtype Handle m =
  Handle
    { hGetNews :: m [News]
    }

getNews :: Handle m -> m [News]
getNews = hGetNews

data News =
  News
    { newsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }
  deriving (Eq, Show)
