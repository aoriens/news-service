module Core.ExactConversion
  ( fromIntegralExact
  ) where

fromIntegralExact :: (Integral a, Integral b) => a -> Maybe b
fromIntegralExact x
  | toInteger x == toInteger r = Just r
  | otherwise = Nothing
  where
    r = fromIntegral x
