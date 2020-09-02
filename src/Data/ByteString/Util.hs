module Data.ByteString.Util
  ( trim
  , trimLeft
  , trimRight
  ) where

import qualified Data.ByteString.Char8 as B
import Data.Char

-- | Strips leading spaces.
trimLeft :: B.ByteString -> B.ByteString
trimLeft = B.dropWhile isSpace

-- | Strips trailing spaces.
trimRight :: B.ByteString -> B.ByteString
trimRight = fst . B.spanEnd isSpace

-- | Strips leading and trailing spaces.
trim :: B.ByteString -> B.ByteString
trim = trimRight . trimLeft
