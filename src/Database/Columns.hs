{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- | Table data decoder, coupled with table column names. This can
-- help to avoid duplication of columns lists.
module Database.Columns
  ( Columns
  , TableName
  , column
  , statementWithColumns
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.DList as DL
import Data.String
import Database.NativeSQLDecodable
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
    { columnsNamesDList :: DL.DList QualifiedColumnName
    , columnsRow :: D.Row a
    }

newtype TableName =
  TableName
    { getTableName :: B.ByteString
    }
  deriving (IsString)

type ColumnName = B.ByteString

type QualifiedColumnName = (TableName, ColumnName)

type SQL = B.ByteString

instance Functor Columns where
  fmap f columns = columns {columnsRow = f <$> columnsRow columns}

instance Applicative Columns where
  pure = Columns mempty . pure
  Columns names1 f <*> Columns names2 a = Columns (names1 <> names2) (f <*> a)

-- | Creates a decoder for the specified table column.
column :: NativeSQLDecodable a => TableName -> ColumnName -> Columns a
column table name = Columns (DL.singleton (table, name)) nativeSQLDecoder

columnsNames :: Columns a -> [QualifiedColumnName]
columnsNames = DL.toList . columnsNamesDList

renderColumns :: Columns a -> SQL
renderColumns = B.intercalate ", " . map renderQualifiedName . columnsNames

renderQualifiedName :: QualifiedColumnName -> SQL
renderQualifiedName (table, name) = getTableName table <> "." <> name

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
      replaceAllSubstrings
        (" " <> renderColumns columns <> " ")
        "$COLUMNS"
        sqlTemplate

replaceAllSubstrings ::
     B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceAllSubstrings new old = go
  where
    go s =
      let (prefix, suffix) = B.breakSubstring old s
       in if B.null suffix
            then prefix
            else prefix <> new <> go (B.drop (B.length old) suffix)
