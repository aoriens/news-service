module Web.Representation.OneOf
  ( OneOfRep(..)
  ) where

import qualified Data.Aeson as A

-- | A type isomorphic to 'Either', which is encoded to JSON just as
-- its subvalues are. 'Either' is encoded differently.
--
-- >>> encode (LeftRep 1 :: OneOfRep Int String)
-- "1"
--
-- >>> encode (RightRep "str" :: OneOfRep Int String)
-- "\"str\""
data OneOfRep a b
  = LeftRep a
  | RightRep b

instance (A.ToJSON a, A.ToJSON b) => A.ToJSON (OneOfRep a b) where
  toJSON (LeftRep a) = A.toJSON a
  toJSON (RightRep b) = A.toJSON b
  toEncoding (LeftRep a) = A.toEncoding a
  toEncoding (RightRep b) = A.toEncoding b
