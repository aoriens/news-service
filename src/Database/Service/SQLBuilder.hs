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
sqlText s =
  SQLBuilder {sqlNodes = DL.singleton $ SQLText s, sqlEncoder = mempty}

sqlParam :: NativeSQLEncodable p => p -> SQLBuilder
sqlParam p =
  SQLBuilder
    { sqlNodes = DL.singleton SQLPlaceholder
    , sqlEncoder = const p >$< nativeSQLEncoder
    }

renderSQLBuilder :: SQLBuilder -> (SQL, E.Params ())
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
