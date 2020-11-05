module Gateway.CurrentTime
  ( getIntegralSecondsTime
  , getCurrentDay
  ) where

import Data.Time

-- | Returns UTCTime with integral (truncated) seconds. It can be
-- useful when storing time into a persistent storage that does not
-- support UTCTime's precision. Inaccuracy in the fractional part of
-- seconds may result in different representations of the same REST
-- entity before and after saving to a database.
getIntegralSecondsTime :: IO UTCTime
getIntegralSecondsTime = truncateSeconds <$> getCurrentTime
  where
    truncateSeconds (UTCTime day time) =
      UTCTime day $ fromIntegral (truncate time :: Int)

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> getCurrentTime
