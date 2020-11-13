module Database.Logic.Pagination
  ( pageToLimitOffsetEncoder
  , pageSpecToLimitOffset
  ) where

import Core.Pagination
import Data.Functor.Contravariant
import qualified Database.Service.SQLBuilder as Sql
import qualified Hasql.Encoders as E

-- | Encodes a page into two SQL parameters for 'LIMIT ... OFFSET ...'
-- clause.
pageToLimitOffsetEncoder :: E.Params PageSpec
pageToLimitOffsetEncoder =
  (getPageLimit . pageLimit >$< (E.param . E.nonNullable) E.int4) <>
  (getPageOffset . pageOffset >$< (E.param . E.nonNullable) E.int4)

pageSpecToLimitOffset :: PageSpec -> Sql.Builder
pageSpecToLimitOffset PageSpec {..} =
  "limit" <> Sql.param (getPageLimit pageLimit) <> "offset" <>
  Sql.param (getPageOffset pageOffset)
