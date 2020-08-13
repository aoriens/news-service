{-# LANGUAGE RecordWildCards #-}

module Core.Interactor.GetNews
  ( Handle(..)
  , getNews
  , News(..)
  ) where

import Core.Pagination
import Data.Int
import Data.Text (Text)
import Data.Time.Calendar

data Handle m =
  Handle
    { hGetNews :: PageLimit -> m [News]
    , hMaxPageLimit :: PageLimit
    }

getNews :: Handle m -> m [News]
getNews Handle {..} = hGetNews hMaxPageLimit

data News =
  News
    { newsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }
  deriving (Eq, Show)
