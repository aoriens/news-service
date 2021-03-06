{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.News
  ( News(..)
  , NewsId(..)
  , Draft(..)
  , DraftId(..)
  , NewsVersion(..)
  ) where

import Core.Author
import Core.Category
import Core.Deletable
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
    , newsContent :: NewsVersion
    }
  deriving (Eq, Show)

newtype NewsId =
  NewsId
    { getNewsId :: Int32
    }
  deriving (Eq, Show)

data Draft =
  Draft
    { draftId :: DraftId
    , draftContent :: NewsVersion
    , draftNewsIdItWasCreatedFrom :: Maybe NewsId
    }
  deriving (Eq, Show)

newtype DraftId =
  DraftId
    { getDraftId :: Int32
    }
  deriving (Eq, Show, Hashable)

data NewsVersion =
  NewsVersion
    { nvTitle :: Text
    , nvText :: Text
    , nvAuthor :: Deletable Author
    , nvCategory :: Maybe Category
    -- ^ The category may be absent, which is considered as the
    -- super-root "uncategorized" category, logically being the parent
    -- of all root categories.
    , nvMainPhotoId :: Maybe ImageId
    , nvAdditionalPhotoIds :: HashSet ImageId
    , nvTags :: HashSet Tag
    }
  deriving (Eq, Show)
