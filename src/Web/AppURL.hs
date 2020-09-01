{-# LANGUAGE RecordWildCards #-}

-- | Type-safe URLs. The module describes mapping between ADT-encoded
-- and text URLs existing in the application, as well as conversions
-- between them. ADT-style URLs are good at finding broken links and
-- easily changing textual representation of them.
module Web.AppURL
  ( AppURL(..)
  , RelativeURL(..)
  , Config(..)
  , render'
  , render
  , toRelativeURL
  , fromRelativeURL
  ) where

import Core.Image
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Integral.Exact
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http

data Config =
  Config
    { cfUseHTTPS :: Bool
    , cfDomain :: BS.ByteString
    }

newtype RelativeURL =
  RelativeURL
    { relativeURLPath :: [Text]
    }

newtype AppURL =
  URLImage ImageId
  deriving (Eq, Show)

render' :: Config -> AppURL -> BS.ByteString
render' = (LBS.toStrict .) . (BB.toLazyByteString .) . render

render :: Config -> AppURL -> BB.Builder
render Config {..} appURL =
  BB.byteString scheme <> BB.byteString cfDomain <> path
  where
    scheme
      | cfUseHTTPS = "https://"
      | otherwise = "http://"
    path = Http.encodePathSegments . relativeURLPath $ toRelativeURL appURL

toRelativeURL :: AppURL -> RelativeURL
toRelativeURL (URLImage (ImageId imageId)) =
  RelativeURL ["image", T.pack $ show imageId]

fromRelativeURL :: RelativeURL -> Maybe AppURL
fromRelativeURL (RelativeURL ["image", ident]) =
  URLImage . ImageId <$> readExactIntegral (T.unpack ident)
fromRelativeURL _ = Nothing
