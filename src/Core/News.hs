module Core.News
  ( News(..)
  , NewsId(..)
  , NewsVersion(..)
  , NewsVersionId(..)
  ) where

import Data.Int
import Data.Text (Text)
import Data.Time

data News =
  News
    { newsId :: NewsId
    , newsDate :: Day
    , newsVersion :: NewsVersion
    }
  deriving (Eq, Show)

newtype NewsId =
  NewsId
    { getNewsId :: Int32
    }
  deriving (Eq, Show)

data NewsVersion =
  NewsVersion
    { nvId :: NewsVersionId
    , nvTitle :: Text
    , nvText :: Text
    }
  deriving (Eq, Show)

newtype NewsVersionId =
  NewsVersionId
    { getNewsVersionId :: Int32
    }
  deriving (Eq, Show)
