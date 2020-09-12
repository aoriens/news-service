module Database.Pagination
  ( pageToLimitOffsetEncoder
  ) where

import Core.Pagination
import Data.Functor.Contravariant
import qualified Hasql.Encoders as E

-- | Encodes a page into two SQL parameters for 'LIMIT ... OFFSET ...'
-- clause.
pageToLimitOffsetEncoder :: E.Params PageSpec
pageToLimitOffsetEncoder =
  (getPageLimit . pageLimit >$< (E.param . E.nonNullable) E.int4) <>
  (getPageOffset . pageOffset >$< (E.param . E.nonNullable) E.int4)
