module Database.Logic.Pagination
  ( pageToLimitOffsetEncoder
  , pageSpecToLimitOffset
  ) where

import Core.Pagination
import Data.Functor.Contravariant
import Database.Service.SQLBuilder
import qualified Hasql.Encoders as E

-- | Encodes a page into two SQL parameters for 'LIMIT ... OFFSET ...'
-- clause.
pageToLimitOffsetEncoder :: E.Params PageSpec
pageToLimitOffsetEncoder =
  (getPageLimit . pageLimit >$< (E.param . E.nonNullable) E.int4) <>
  (getPageOffset . pageOffset >$< (E.param . E.nonNullable) E.int4)

pageSpecToLimitOffset :: PageSpec -> SQLBuilder
pageSpecToLimitOffset PageSpec {..} =
  "limit" <> sqlParam (getPageLimit pageLimit) <> "offset" <>
  sqlParam (getPageOffset pageOffset)
