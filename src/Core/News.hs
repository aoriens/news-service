module Core.News
  ( News(..)
  , NewsId(..)
  ) where

import Data.Int
import Data.Text (Text)
import Data.Time

data News =
  News
    { newsId :: NewsId
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }
  deriving (Eq, Show)

newtype NewsId =
  NewsId
    { getNewsId :: Int32
    }
  deriving (Eq, Show)
