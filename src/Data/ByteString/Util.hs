module Data.ByteString.Util
  ( trim
  , trimLeft
  , trimRight
  , splitOnCharOnce
  , replaceAllSubstrings
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

-- | Splits the string on the first byte satisfying the predicate and
-- returns the prefix and the suffix, if the byte is found.
splitOnCharOnce ::
     (Char -> Bool) -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
splitOnCharOnce charPredicate s =
  let (prefix, rest) = B.break charPredicate s
   in case B.uncons rest of
        Just (_, suffix) -> Just (prefix, suffix)
        Nothing -> Nothing

replaceAllSubstrings ::
     B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceAllSubstrings new old = go
  where
    go s =
      let (prefix, suffix) = B.breakSubstring old s
       in if B.null suffix
            then prefix
            else prefix <> new <> go (B.drop (B.length old) suffix)
