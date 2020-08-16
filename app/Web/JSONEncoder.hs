module Web.JSONEncoder
  ( encode
  , Config(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TL

newtype Config =
  Config
    { prettyPrint :: Bool
    }

encode :: A.ToJSON a => Config -> a -> BB.Builder
encode config
  | prettyPrint config =
    BB.lazyByteString .
    TL.encodeUtf8 .
    TB.toLazyText . Pretty.encodePrettyToTextBuilder' prettyConfig
  | otherwise = A.fromEncoding . A.toEncoding
  where
    prettyConfig = Pretty.defConfig {Pretty.confTrailingNewline = True}
