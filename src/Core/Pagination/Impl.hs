module Core.Pagination.Impl
  ( new
  ) where

import Core.Pagination

-- | Creates a Handle. The parameter is the maximum 'PageLimit'
-- allowed.
new :: PageLimit -> PagerHandle
new maxLimit =
  PagerHandle $ \PageQuery {..} -> do
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
