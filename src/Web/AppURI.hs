-- | Type-safe URIs. The module describes mapping between ADT-encoded
-- and text URIs existing in the application. ADT-style URIs are good
-- at finding broken links and easily changing textual representation
-- of them.
module Web.AppURI
  ( AppURI(..)
  , RelativeURI(..)
  , Config(..)
  , render
  , toRelativeURI
  , fromRelativeURI
  ) where

import Core.Image
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Integral.Exact
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as Http

data Config =
  Config
    { cfUseHTTPS :: Bool
    , cfDomain :: T.Text
    }

newtype RelativeURI =
  RelativeURI
    { relativeURIPath :: [Text]
    }

newtype AppURI =
  URIImage ImageId
  deriving (Eq, Show)

render :: Config -> AppURI -> T.Text
render Config {..} appURI = scheme <> cfDomain <> path
  where
    scheme
      | cfUseHTTPS = "https://"
      | otherwise = "http://"
    path =
      T.decodeUtf8 . buildByteString . Http.encodePathSegments . relativeURIPath $
      toRelativeURI appURI

buildByteString :: BB.Builder -> B.ByteString
buildByteString = LB.toStrict . BB.toLazyByteString

toRelativeURI :: AppURI -> RelativeURI
toRelativeURI (URIImage (ImageId imageId)) =
  RelativeURI ["image", T.pack $ show imageId]

fromRelativeURI :: RelativeURI -> Maybe AppURI
fromRelativeURI (RelativeURI ["image", ident]) =
  URIImage . ImageId <$> readExactIntegral (T.unpack ident)
fromRelativeURI _ = Nothing
