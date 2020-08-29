module Web.Entity.Base64
  ( Base64(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | An entity able to decode and encode a bytestring in base64
-- format.
newtype Base64 =
  Base64
    { unBase64 :: BS.ByteString
    }

instance A.ToJSON Base64 where
  toJSON = A.String . B64.encodeBase64 . unBase64
  toEncoding = A.text . B64.encodeBase64 . unBase64

instance A.FromJSON Base64 where
  parseJSON =
    A.withText "base64 string" $
    either (fail . T.unpack) (pure . Base64) . B64.decodeBase64 . T.encodeUtf8
