module Core.Pagination
  ( PageSpec(..)
  , PageSpecQuery(..)
  , PageLimit(..)
  , PageOffset(..)
  , PageSpecParserHandle(..)
  , parsePageSpecM
  ) where

import Control.Monad.Catch
import Core.Exception
import Data.Int
import Data.Text

-- | The part of data to be output in a response.
data PageSpec =
  PageSpec
    { pageOffset :: PageOffset
    , pageLimit :: PageLimit
    }
  deriving (Eq, Show)

-- | A variant of 'PageSpec' with unchecked, raw data from the user. It
-- may be used as an input type in interactors.
data PageSpecQuery =
  PageSpecQuery
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

newtype PageSpecParserHandle =
  PageSpecParserHandle
    { parsePageSpec :: PageSpecQuery -> Either Text PageSpec
    -- ^ Converts a page query to a page. Returns Left if the page
    -- query is incorrect.
    }

-- | Performs 'parsePageSpec and throws 'CoreException' in case of
-- incorrect 'PageSpecQuery'.
parsePageSpecM ::
     MonadThrow m => PageSpecParserHandle -> PageSpecQuery -> m PageSpec
parsePageSpecM config pageQuery =
  either (throwM . QueryException) pure (parsePageSpec config pageQuery)
