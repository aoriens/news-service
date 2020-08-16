module Web.JSONEncoder
  ( encode
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB

encode :: A.ToJSON a => a -> BB.Builder
encode = A.fromEncoding . A.toEncoding
