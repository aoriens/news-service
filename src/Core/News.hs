module Core.News
  ( News(..)
  ) where

import Data.Int
import Data.Text (Text)
import Data.Time

data News =
  News
    { newsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }
  deriving (Eq, Show)
