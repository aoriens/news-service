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
import Core.Category
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
  | CategoryURI CategoryId
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
toRelativeURI uri =
  RelativeURI $
  case uri of
    ImageURI (ImageId imageId) -> ["images", T.pack $ show imageId]
    UserURI (UserId userId) -> ["users", T.pack $ show userId]
    AuthorURI (AuthorId authorId) -> ["authors", T.pack $ show authorId]
    CategoryURI (CategoryId catId) -> ["categories", T.pack $ show catId]

fromRelativeURI :: RelativeURI -> Maybe AppURI
fromRelativeURI (RelativeURI path) =
  case path of
    ["images", ident] ->
      ImageURI . ImageId <$> readExactIntegral (T.unpack ident)
    ["users", ident] -> UserURI . UserId <$> readExactIntegral (T.unpack ident)
    ["authors", ident] ->
      AuthorURI . AuthorId <$> readExactIntegral (T.unpack ident)
    ["categories", ident] ->
      CategoryURI . CategoryId <$> readExactIntegral (T.unpack ident)
    _ -> Nothing
