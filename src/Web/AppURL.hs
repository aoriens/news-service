-- | Type-safe URLs. The module describes mapping between ADT-encoded
-- and text URLs existing in the application. ADT-style URLs are good
-- at finding broken links and easily changing textual representation
-- of them.
module Web.AppURL
  ( AppURL(..)
  , RelativeURL(..)
  , Config(..)
  , render
  , toRelativeURL
  , fromRelativeURL
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

newtype RelativeURL =
  RelativeURL
    { relativeURLPath :: [Text]
    }

newtype AppURL =
  URLImage ImageId
  deriving (Eq, Show)

render :: Config -> AppURL -> T.Text
render Config {..} appURL = scheme <> cfDomain <> path
  where
    scheme
      | cfUseHTTPS = "https://"
      | otherwise = "http://"
    path =
      T.decodeUtf8 . buildByteString . Http.encodePathSegments . relativeURLPath $
      toRelativeURL appURL

buildByteString :: BB.Builder -> B.ByteString
buildByteString = LB.toStrict . BB.toLazyByteString

toRelativeURL :: AppURL -> RelativeURL
toRelativeURL (URLImage (ImageId imageId)) =
  RelativeURL ["image", T.pack $ show imageId]

fromRelativeURL :: RelativeURL -> Maybe AppURL
fromRelativeURL (RelativeURL ["image", ident]) =
  URLImage . ImageId <$> readExactIntegral (T.unpack ident)
fromRelativeURL _ = Nothing
