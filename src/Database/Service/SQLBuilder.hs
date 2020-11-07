module Database.Service.SQLBuilder
  ( SQLBuilder
  , sqlText
  , sqlParam
  , renderSQLBuilder
  , NativeSQLEncodable
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.DList as DL
import Data.Functor.Contravariant
import Data.Int
import Data.String
import Data.Time
import Data.Traversable
import qualified Hasql.Encoders as E

type SQL = B.ByteString

data SQLBuilder in' =
  SQLBuilder
    { sqlNodes :: !(DL.DList SQLNode)
    , sqlEncoder :: !(E.Params in')
    }

data SQLNode
  = SQLText SQL
  | SQLPlaceholder

instance Contravariant SQLBuilder where
  contramap f x = x {sqlEncoder = contramap f $ sqlEncoder x}

instance Semigroup (SQLBuilder in') where
  a <> b =
    SQLBuilder
      { sqlEncoder = sqlEncoder a <> sqlEncoder b
      , sqlNodes = sqlNodes a <> sqlNodes b
      }

instance Monoid (SQLBuilder in') where
  mempty = SQLBuilder {sqlEncoder = mempty, sqlNodes = mempty}

instance IsString (SQLBuilder in') where
  fromString = sqlText . fromString

sqlText :: SQL -> SQLBuilder a
sqlText s =
  SQLBuilder {sqlNodes = DL.singleton $ SQLText s, sqlEncoder = mempty}

sqlParam :: NativeSQLEncodable p => SQLBuilder p
sqlParam =
  SQLBuilder
    {sqlNodes = DL.singleton SQLPlaceholder, sqlEncoder = nativeSQLEncoder}

renderSQLBuilder :: SQLBuilder in' -> (SQL, E.Params in')
renderSQLBuilder SQLBuilder {..} = (B.intercalate " " segments, sqlEncoder)
  where
    (_, segments) = mapAccumL f 1 $ DL.toList sqlNodes
    f :: Int -> SQLNode -> (Int, SQL)
    f n (SQLText text) = (n, text)
    f n SQLPlaceholder = (succ $! n, "$" <> B.pack (show n))

class NativeSQLEncodable a where
  nativeSQLEncoder :: E.Params a

instance NativeSQLEncodable Day where
  nativeSQLEncoder = (E.param . E.nonNullable) E.date

instance NativeSQLEncodable Int32 where
  nativeSQLEncoder = (E.param . E.nonNullable) E.int4
