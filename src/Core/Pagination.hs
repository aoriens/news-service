module Core.Pagination
  ( PageLimit(..)
  ) where

import Data.Int

-- | The maximum number of items to be output on a page.
newtype PageLimit =
  PageLimit
    { getPageLimit :: Int32
    }
  deriving (Eq, Ord)
