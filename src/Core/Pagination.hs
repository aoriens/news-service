module Core.Pagination
  ( Page(..)
  , PageQuery(..)
  , PageLimit(..)
  , PageOffset(..)
  , PagerHandle(..)
  , pageFromPageQueryM
  ) where

import Control.Monad.Catch
import Core.Exception
import Data.Int

-- | The part of data to be output in a response.
data Page =
  Page
    { pageOffset :: PageOffset
    , pageLimit :: PageLimit
    }
  deriving (Eq, Show)

-- | A variant of 'Page' with unchecked, raw data from the user. It
-- may be used as an input type in interactors.
data PageQuery =
  PageQuery
    { pageQueryOffset :: Maybe Int32
    , pageQueryLimit :: Maybe Int32
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

newtype PagerHandle =
  PagerHandle
    { pageFromPageQuery :: PageQuery -> Maybe Page
    -- ^ Converts a page query to a page. Returns Nothing if the page
    -- query is incorrect.
    }

-- | Performs 'pageFromPageQuery and throws 'CoreException' in case of
-- incorrect 'PageQuery'.
pageFromPageQueryM :: MonadThrow m => PagerHandle -> PageQuery -> m Page
pageFromPageQueryM config pageQuery =
  maybe (throwM pageException) pure (pageFromPageQuery config pageQuery)
  where
    pageException =
      QueryException "Invalid pagination parameters: offset or limit"
