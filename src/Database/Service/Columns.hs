{-# LANGUAGE RankNTypes #-}

-- | Table data decoder, coupled with table column names. This can
-- help to avoid duplication of columns lists.
module Database.Service.Columns
  ( Columns
  , TableName
  , column
  , noTable
  , statementWithColumns
  , runStatementWithColumns
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Util as B
import qualified Data.DList as DL
import Data.String
import Database.Service.NativeSQLDecodable
import Database.Service.Primitives
import qualified Database.Service.SQLBuilder as Sql
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Statement as S

-- | The data decoder functor. It contains a 'D.Row' decoder, as well
-- as table column names to decode from. It allows to build SQL
-- statements with convenient decoders to high-level data types
-- without having to duplicate column names. Since it is an instance
-- of 'Applicative', it allows building more complex column lists
-- using @ApplicativeDo@.
data Columns a =
  Columns
    { columnsNamesDList :: DL.DList FullColumnName
    , columnsRow :: D.Row a
    }

newtype TableName =
  TableName (Maybe B.ByteString)

instance IsString TableName where
  fromString = TableName . Just . fromString

type ColumnName = B.ByteString

type FullColumnName = (TableName, ColumnName)

type SQL = B.ByteString

instance Functor Columns where
  fmap f columns = columns {columnsRow = f <$> columnsRow columns}

instance Applicative Columns where
  pure = Columns mempty . pure
  Columns names1 f <*> Columns names2 a = Columns (names1 <> names2) (f <*> a)

-- | Creates a decoder for the specified table column.
column :: NativeSQLDecodable a => TableName -> ColumnName -> Columns a
column table name = Columns (DL.singleton (table, name)) nativeSQLDecoder

-- | A fake table name to mean an absent table. A column for this
-- table will be rendered unqualified. Similar tasks are usually
-- solved using 'Maybe', but using 'Just' would make 'column' usage
-- less convenient and readable.
noTable :: TableName
noTable = TableName Nothing

columnsNames :: Columns a -> [FullColumnName]
columnsNames = DL.toList . columnsNamesDList

renderColumns :: Columns a -> SQL
renderColumns = B.intercalate ", " . map renderFullColumnName . columnsNames

renderFullColumnName :: FullColumnName -> SQL
renderFullColumnName =
  \case
    (TableName (Just table), name) -> table <> "." <> name
    (TableName Nothing, name) -> name

-- Creates statement with 'Columns'. SQL strings may contain
-- @$COLUMNS@ token that will be replaced with the comma-separated
-- columns.
statementWithColumns ::
     SQL
  -> E.Params a
  -> Columns b
  -> (D.Row b -> D.Result c)
  -> Bool
  -> S.Statement a c
statementWithColumns sqlTemplate encoder columns resultMaker =
  S.Statement sql encoder decoder
  where
    decoder = resultMaker $ columnsRow columns
    sql =
      B.replaceAllSubstrings
        (" " <> renderColumns columns <> " ")
        "$COLUMNS"
        sqlTemplate

runStatementWithColumns ::
     Sql.Builder
  -> Columns b
  -> (D.Row b -> D.Result c)
  -> Bool
  -> Transaction c
runStatementWithColumns sqlBuilder columns resultMaker shouldPrepare =
  statement `runStatement` ()
  where
    statement =
      statementWithColumns sql encoder columns resultMaker shouldPrepare
    (sql, encoder) = Sql.renderBuilder sqlBuilder
