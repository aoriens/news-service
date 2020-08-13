module Interactor.GetNews
  ( Handle(..)
  , getNews
  , News(..)
  ) where

import Data.Text (Text)
import Data.Time.Calendar
import Data.Int

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
