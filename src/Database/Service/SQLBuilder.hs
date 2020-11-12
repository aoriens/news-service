{-# LANGUAGE FlexibleInstances #-}

module Database.Service.SQLBuilder
  ( SQLBuilder
  , sqlText
  , sqlParam
  , sqlBuilderIsEmpty
  , ifSQLBuilderEmpty
  , ifSQLBuilderEmptyOr
  , renderSQLBuilder
  , NativeSQLEncodable
  , nativeSQLEncoder
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.DList as DL
import Data.Foldable
import Data.Functor.Contravariant
import Data.Int
import Data.String
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import qualified Hasql.Encoders as E

type SQL = B.ByteString

data SQLBuilder =
  SQLBuilder
    { sqlNodes :: !(DL.DList SQLNode)
    , sqlEncoder :: !(E.Params ())
    }

data SQLNode
  = SQLText SQL
  | SQLPlaceholder

instance Semigroup SQLBuilder where
  a <> b =
    SQLBuilder
      { sqlEncoder = sqlEncoder a <> sqlEncoder b
      , sqlNodes = sqlNodes a <> sqlNodes b
      }

instance Monoid SQLBuilder where
  mempty = SQLBuilder {sqlEncoder = mempty, sqlNodes = mempty}

instance IsString SQLBuilder where
  fromString = sqlText . fromString

sqlText :: SQL -> SQLBuilder
sqlText s
  | B.null s = mempty
  | otherwise =
    SQLBuilder {sqlNodes = DL.singleton $ SQLText s, sqlEncoder = mempty}

sqlParam :: NativeSQLEncodable p => p -> SQLBuilder
sqlParam = sqlParamFrom id

sqlParamFrom :: NativeSQLEncodable p => (a -> p) -> a -> SQLBuilder
sqlParamFrom f a =
  SQLBuilder
    { sqlNodes = DL.singleton SQLPlaceholder
    , sqlEncoder = const (f a) >$< nativeSQLEncoder
    }

sqlBuilderIsEmpty :: SQLBuilder -> Bool
sqlBuilderIsEmpty = null . DL.toList . sqlNodes

ifSQLBuilderEmpty :: SQLBuilder -> SQLBuilder -> SQLBuilder
ifSQLBuilderEmpty x y
  | sqlBuilderIsEmpty x = y
  | otherwise = x

ifSQLBuilderEmptyOr :: a -> (SQLBuilder -> a) -> SQLBuilder -> a
ifSQLBuilderEmptyOr ifEmpty ifNonEmpty b
  | sqlBuilderIsEmpty b = ifEmpty
  | otherwise = ifNonEmpty b

renderSQLBuilder :: SQLBuilder -> (SQL, E.Params ())
renderSQLBuilder SQLBuilder {..} = (B.intercalate " " segments, sqlEncoder)
  where
    (_, segments) = mapAccumL f 1 $ DL.toList sqlNodes
    f :: Int -> SQLNode -> (Int, SQL)
    f n (SQLText text) = (n, text)
    f n SQLPlaceholder = (succ $! n, "$" <> B.pack (show n))

class NativeSQLEncodable a where
  nativeSQLNullabilityEncoder :: E.NullableOrNot E.Value a

nativeSQLEncoder :: NativeSQLEncodable a => E.Params a
nativeSQLEncoder = E.param nativeSQLNullabilityEncoder

instance NativeSQLEncodable Day where
  nativeSQLNullabilityEncoder = E.nonNullable E.date

instance NativeSQLEncodable Int32 where
  nativeSQLNullabilityEncoder = E.nonNullable E.int4

instance NativeSQLEncodable T.Text where
  nativeSQLNullabilityEncoder = E.nonNullable E.text

instance (Foldable t, NativeSQLEncodable a) => NativeSQLEncodable (t a) where
  nativeSQLNullabilityEncoder =
    E.nonNullable . E.array . E.dimension foldl' $
    E.element nativeSQLNullabilityEncoder
