{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.News
  ( News(..)
  , NewsId(..)
  , NewsVersion(..)
  , NewsVersionId(..)
  ) where

import Core.Author
import Core.Category
import Core.Image
import Core.Tag
import Data.HashSet (HashSet)
import Data.Hashable
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
    , nvAuthor :: Author
    , nvCategory :: Category
    , nvMainPhotoId :: Maybe ImageId
    , nvAdditionalPhotoIds :: HashSet ImageId
    , nvTags :: HashSet Tag
    }
  deriving (Eq, Show)

newtype NewsVersionId =
  NewsVersionId
    { getNewsVersionId :: Int32
    }
  deriving (Eq, Show, Hashable)
