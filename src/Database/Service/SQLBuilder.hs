{-# LANGUAGE FlexibleInstances #-}

-- The SQL builder. The module should be imported qualified.
module Database.Service.SQLBuilder
  ( Builder
  , text
  , param
  , isEmpty
  , ifEmpty
  , mapNonEmpty
  , renderBuilder
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

data Builder =
  Builder
    { sqlNodes :: !(DL.DList SQLNode)
    , sqlEncoder :: !(E.Params ())
    }

data SQLNode
  = SQLText SQL
  | SQLPlaceholder

instance Semigroup Builder where
  a <> b =
    Builder
      { sqlEncoder = sqlEncoder a <> sqlEncoder b
      , sqlNodes = sqlNodes a <> sqlNodes b
      }

instance Monoid Builder where
  mempty = Builder {sqlEncoder = mempty, sqlNodes = mempty}

instance IsString Builder where
  fromString = text . fromString

text :: SQL -> Builder
text s
  | B.null s = mempty
  | otherwise =
    Builder {sqlNodes = DL.singleton $ SQLText s, sqlEncoder = mempty}

param :: NativeSQLEncodable p => p -> Builder
param = paramFrom id

paramFrom :: NativeSQLEncodable p => (a -> p) -> a -> Builder
paramFrom f a =
  Builder
    { sqlNodes = DL.singleton SQLPlaceholder
    , sqlEncoder = const (f a) >$< nativeSQLEncoder
    }

isEmpty :: Builder -> Bool
isEmpty = null . DL.toList . sqlNodes

ifEmpty :: Builder -> Builder -> Builder
ifEmpty x y
  | isEmpty x = y
  | otherwise = x

mapNonEmpty :: (Builder -> Builder) -> Builder -> Builder
mapNonEmpty f b
  | isEmpty b = b
  | otherwise = f b

renderBuilder :: Builder -> (SQL, E.Params ())
renderBuilder Builder {..} = (B.intercalate " " segments, sqlEncoder)
  where
    (_, segments) = mapAccumL f 1 $ DL.toList sqlNodes
    f :: Int -> SQLNode -> (Int, SQL)
    f n (SQLText text') = (n, text')
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
