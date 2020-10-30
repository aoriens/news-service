{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Representation.AppURI
  ( AppURIRep(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

-- | A JSON-encodable type-safe wrapper for application URLs.
newtype AppURIRep =
  AppURIRep T.Text
  deriving (A.ToJSON)
