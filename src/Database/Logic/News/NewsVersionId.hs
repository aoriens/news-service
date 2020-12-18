module Database.Logic.News.NewsVersionId
  ( NewsVersionId(..)
  ) where

import Data.Int

newtype NewsVersionId =
  NewsVersionId
    { getNewsVersionId :: Int32
    }
  deriving (Eq, Show)
