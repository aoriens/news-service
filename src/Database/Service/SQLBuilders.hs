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
import qualified Database.Service.SQLBuilder as Sql
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
any :: Sql.Builder -> Sql.Builder
any e = "any (" <> e <> ")"

-- | Creates OR expression to combine two SQL expressions. If either
-- expression is empty, OR is not used and just another expression is
-- returned.
or :: Sql.Builder -> Sql.Builder -> Sql.Builder
or = binaryOperationIfNonEmpty "or"

-- | Creates AND expression to combine two SQL expressions. If either
-- expression is empty, AND is not used and just another expression is
-- returned.
and :: Sql.Builder -> Sql.Builder -> Sql.Builder
and = binaryOperationIfNonEmpty "and"

binaryOperationIfNonEmpty ::
     Sql.Builder -> Sql.Builder -> Sql.Builder -> Sql.Builder
binaryOperationIfNonEmpty op x y
  | Sql.isEmpty x = y
  | Sql.isEmpty y = x
  | otherwise = x <> op <> y

between :: Sql.Builder -> (Sql.Builder, Sql.Builder) -> Sql.Builder
between expr (from, to) = expr <> "between" <> from <> "and" <> to

equal :: Sql.Builder -> Sql.Builder -> Sql.Builder
equal x y = x <> "=" <> y

greaterOrEqual :: Sql.Builder -> Sql.Builder -> Sql.Builder
greaterOrEqual x y = x <> ">=" <> y

lessOrEqual :: Sql.Builder -> Sql.Builder -> Sql.Builder
lessOrEqual x y = x <> "<=" <> y
