module Database.Service.SQLBuilders
  ( escapeLikePattern
  , any
  , or
  , and
  , between
  , equal
  , lessOrEqual
  , greaterOrEqual
  ) where

import qualified Data.Text as T
import Database.Service.SQLBuilder
import Prelude hiding (and, any, or)

-- | Converts a string into a literal text in SQL LIKE predicate
-- format. It may be used to create LIKE patterns based on
-- user-entered text.
escapeLikePattern :: T.Text -> T.Text
escapeLikePattern = T.concatMap f
  where
    f char
      | shouldEscape char = escapeChar `T.cons` T.singleton char
      | otherwise = T.singleton char
    shouldEscape char = char == escapeChar || char == '%' || char == '_'
    escapeChar = '\\'

-- | Wraps an expression into ANY (...) expression.
any :: SQLBuilder -> SQLBuilder
any e = "any (" <> e <> ")"

-- | Creates OR expression to combine two SQL expressions. If either
-- expression is empty, OR is not used and just another expression is
-- returned.
or :: SQLBuilder -> SQLBuilder -> SQLBuilder
or = binaryOperationIfNonEmpty "or"

-- | Creates AND expression to combine two SQL expressions. If either
-- expression is empty, AND is not used and just another expression is
-- returned.
and :: SQLBuilder -> SQLBuilder -> SQLBuilder
and = binaryOperationIfNonEmpty "and"

binaryOperationIfNonEmpty ::
     SQLBuilder -> SQLBuilder -> SQLBuilder -> SQLBuilder
binaryOperationIfNonEmpty op x y
  | sqlBuilderIsEmpty x = y
  | sqlBuilderIsEmpty y = x
  | otherwise = x <> op <> y

between :: SQLBuilder -> (SQLBuilder, SQLBuilder) -> SQLBuilder
between expr (from, to) = expr <> "between" <> from <> "and" <> to

equal :: SQLBuilder -> SQLBuilder -> SQLBuilder
equal x y = x <> "=" <> y

greaterOrEqual :: SQLBuilder -> SQLBuilder -> SQLBuilder
greaterOrEqual x y = x <> ">=" <> y

lessOrEqual :: SQLBuilder -> SQLBuilder -> SQLBuilder
lessOrEqual x y = x <> "<=" <> y
