-- | Type-safe URIs. The module describes mapping between ADT-encoded
-- and text URIs existing in the application. ADT-style URIs are good
-- at finding broken links and easily changing textual representation
-- of them.
module Web.AppURI
  ( AppURI(..)
  , RelativeURI(..)
  , AppURIConfig(..)
  , renderAppURI
  , toRelativeURI
  , fromRelativeURI
  ) where

import Core.Author
import Core.Image
import Core.User
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Integral.Exact
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as Http

data AppURIConfig =
  AppURIConfig
    { cfUseHTTPS :: Bool
    , cfDomain :: T.Text
    }

newtype RelativeURI =
  RelativeURI
    { relativeURIPath :: [Text]
    }

data AppURI
  = ImageURI ImageId
  | UserURI UserId
  | AuthorURI AuthorId
  deriving (Eq, Show)

renderAppURI :: AppURIConfig -> AppURI -> T.Text
renderAppURI AppURIConfig {..} appURI = scheme <> cfDomain <> path
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
toRelativeURI (ImageURI (ImageId imageId)) =
  RelativeURI ["images", T.pack $ show imageId]
toRelativeURI (UserURI (UserId userId)) =
  RelativeURI ["users", T.pack $ show userId]
toRelativeURI (AuthorURI (AuthorId authorId)) =
  RelativeURI ["authors", T.pack $ show authorId]

fromRelativeURI :: RelativeURI -> Maybe AppURI
fromRelativeURI (RelativeURI ["images", ident]) =
  ImageURI . ImageId <$> readExactIntegral (T.unpack ident)
fromRelativeURI (RelativeURI ["users", ident]) =
  UserURI . UserId <$> readExactIntegral (T.unpack ident)
fromRelativeURI (RelativeURI ["authors", ident]) =
  AuthorURI . AuthorId <$> readExactIntegral (T.unpack ident)
fromRelativeURI _ = Nothing
