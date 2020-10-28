{-# LANGUAGE FlexibleInstances #-}

module Database.NativeSQLDecodable
  ( NativeSQLDecodable(..)
  ) where

import Data.Int
import Data.Text (Text)
import Data.Time
import qualified Hasql.Decoders as D

-- | A class of types provided with built-in Hasql column decoders. It
-- is a convenient, type inference based alternative to the cumbersome
-- Hasql decoder interface, but it is not intended for direct use,
-- consider 'Database.Columns' instead. The instance list might be
-- incomplete, feel free to extend it.
class NativeSQLDecodable a where
  nativeSQLDecoder :: D.Row a

instance NativeSQLDecodable Bool where
  nativeSQLDecoder = D.column $ D.nonNullable D.bool

instance NativeSQLDecodable (Maybe Bool) where
  nativeSQLDecoder = D.column $ D.nullable D.bool

instance NativeSQLDecodable Int32 where
  nativeSQLDecoder = D.column $ D.nonNullable D.int4

instance NativeSQLDecodable (Maybe Int32) where
  nativeSQLDecoder = D.column $ D.nullable D.int4

instance NativeSQLDecodable Text where
  nativeSQLDecoder = D.column $ D.nonNullable D.text

instance NativeSQLDecodable (Maybe Text) where
  nativeSQLDecoder = D.column $ D.nullable D.text

instance NativeSQLDecodable UTCTime where
  nativeSQLDecoder = D.column $ D.nonNullable D.timestamptz

instance NativeSQLDecodable (Maybe UTCTime) where
  nativeSQLDecoder = D.column $ D.nullable D.timestamptz

instance NativeSQLDecodable Day where
  nativeSQLDecoder = D.column $ D.nonNullable D.date

instance NativeSQLDecodable (Maybe Day) where
  nativeSQLDecoder = D.column $ D.nullable D.date
