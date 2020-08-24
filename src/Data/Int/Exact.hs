module Data.Int.Exact
  ( fromIntegralExact
  , parseExactInt
  , parseExactInt32
  ) where

import Data.Int
import Data.Maybe
import Text.Read

-- | Converts an integral to another integral, saving the exact value.
-- If no exact conversion is possible, returns Nothing.
fromIntegralExact :: (Integral a, Integral b) => a -> Maybe b
fromIntegralExact x
  | toInteger x == toInteger r = Just r
  | otherwise = Nothing
  where
    r = fromIntegral x

-- | Parses a string as an 'Int32'. Discards values out of range.
parseExactInt32 :: String -> Maybe Int32
parseExactInt32 = parseIntegral

-- | Parses a string as an 'Int'. Discards values out of range.
parseExactInt :: String -> Maybe Int
parseExactInt = parseIntegral

-- | Parses a string into an integral number. It rejects values out of
-- range and too long strings in order to avoid unneeded computation.
--
-- Not so fast, but simple.
parseIntegral :: (Integral a, Bounded a) => String -> Maybe a
parseIntegral s = r
  where
    r
      | atLeastLong (maxLength + 1) s = Nothing
      | otherwise = fromIntegralExact =<< (readMaybe s :: Maybe Integer)
    maxLength = maxLengthOfNum (maxBound `asTypeOf` fromJust r)

atLeastLong :: Int -> [a] -> Bool
atLeastLong 0 _ = True
atLeastLong n xs = not . null $ drop (n - 1) xs

maxLengthOfNum :: Real a => a -> Int
maxLengthOfNum x = 2 + ceiling (logBase 10 (realToFrac x :: Double))
  -- Adding one digit for sign and one more for calculation inaccuracy
