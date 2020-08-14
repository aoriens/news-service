{-# LANGUAGE RecordWildCards #-}

module Core.Pagination
  ( Page(..)
  , PageQuery(..)
  , PageLimit(..)
  , PageOffset(..)
  , fromPageQuery
  ) where

import Data.Int
import Data.Maybe

-- | The part of data to be output in a response.
data Page =
  Page
    { pageOffset :: PageOffset
    , pageLimit :: PageLimit
    }
  deriving (Eq, Show)

-- | Resembles 'Page', but allows fields to be optional, since they
-- may be missing from a request. It may be used as an input type in
-- interactors.
data PageQuery =
  PageQuery
    { pageQueryOffset :: Maybe PageOffset
    , pageQueryLimit :: Maybe PageLimit
    }
  deriving (Eq, Show)

-- | The index of the first item to output in a response
newtype PageOffset =
  PageOffset
    { getPageOffset :: Int32
    }
  deriving (Eq, Ord, Show)

-- | The maximum number of items to be output on a page.
newtype PageLimit =
  PageLimit
    { getPageLimit :: Int32
    }
  deriving (Eq, Ord, Show)

-- | Convert a page query to a page using maximum limit in the first
-- parameter.
fromPageQuery :: PageLimit -> PageQuery -> Page
fromPageQuery maxLimit PageQuery {..} =
  Page
    { pageLimit = maybe maxLimit (min maxLimit) pageQueryLimit
    , pageOffset = fromMaybe (PageOffset 0) pageQueryOffset
    }
