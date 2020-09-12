module Core.Pagination.Impl
  ( new
  ) where

import Core.Pagination

-- | Creates a Handle. The parameter is the maximum 'PageLimit'
-- allowed.
new :: PageLimit -> PageSpecParserHandle
new maxLimit =
  PageSpecParserHandle $ \PageSpecQuery {..} -> do
    pageLimit <-
      case pageQueryLimit of
        Just limit
          | limit >= 0 -> Right $ min maxLimit (PageLimit limit)
          | otherwise -> Left "Pagination limit must not be negative"
        Nothing -> Right maxLimit
    pageOffset <-
      case pageQueryOffset of
        Just offset
          | offset >= 0 -> Right $ PageOffset offset
          | otherwise -> Left "Pagination offset must not be negative"
        Nothing -> Right $ PageOffset 0
    pure PageSpec {..}
