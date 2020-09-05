module Core.Pagination
  ( Page(..)
  , PageQuery(..)
  , PageLimit(..)
  , PageOffset(..)
  , Config(..)
  , pageFromPageQuery
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

-- | Resembles 'Page', but allows fields to be optional, since they
-- may be missing from a request. It may be used as an input type in
-- interactors.
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

newtype Config =
  Config
    { cfMaxPageLimit :: PageLimit
    }

-- | Converts a page query to a page keeping maximum limit specified
-- in the first parameter. Nothing is returned in case of invalid
-- (e.g. negative) field values.
pageFromPageQuery :: Config -> PageQuery -> Maybe Page
pageFromPageQuery (Config maxLimit) PageQuery {..} = do
  pageLimit <-
    case pageQueryLimit of
      Just limit
        | limit >= 0 -> Just $ min maxLimit (PageLimit limit)
        | otherwise -> Nothing
      Nothing -> Just maxLimit
  pageOffset <-
    case pageQueryOffset of
      Just offset
        | offset >= 0 -> Just $ PageOffset offset
        | otherwise -> Nothing
      Nothing -> Just $ PageOffset 0
  pure Page {..}

-- | Performs 'fromPageQuery' and throws 'QueryException' in case of
-- incorrect 'PageQuery'.
pageFromPageQueryM :: MonadThrow m => Config -> PageQuery -> m Page
pageFromPageQueryM config pageQuery =
  maybe (throwM pageException) pure (pageFromPageQuery config pageQuery)
  where
    pageException =
      QueryException "Invalid pagination parameters: offset or limit"
