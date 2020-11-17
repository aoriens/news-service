-- | Type-safe URIs. The module describes mapping between ADT-encoded
-- and text URIs existing in the application. ADT-style URIs are good
-- at finding broken links and easily changing textual representation
-- of them.
module Web.AppURI
  ( AppURI(..)
  , RelativeURI(..)
  , AppURIConfig(..)
  , renderAppURI
  , parseAppURI
  , toRelativeURI
  , fromRelativeURI
  ) where

import Control.Applicative
import Control.Monad
import Core.Author
import Core.Category
import Core.Comment
import Core.Image
import Core.News
import Core.Tag
import Core.User
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Integral.Exact
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as Http
import qualified Network.URI as URI

data AppURIConfig =
  AppURIConfig
    { cfUseHTTPS :: Bool
    , cfDomain :: T.Text
    , cfPort :: Maybe T.Text
    }

newtype RelativeURI =
  RelativeURI
    { relativeURIPath :: [Text]
    }

data AppURI
  = ImageURI ImageId
  | UsersURI
  | UserURI UserId
  | AuthorsURI
  | AuthorURI AuthorId
  | CategoriesURI
  | CategoryURI CategoryId
  | NewsURI
  | NewsItemURI NewsId
  | TagsURI
  | TagURI TagId
  | DraftsURI
  | DraftURI NewsVersionId
  | PublishDraftURI NewsVersionId
  | CommentsURI NewsId
  | CommentURI NewsId CommentId
  deriving (Eq, Show)

renderAppURI :: AppURIConfig -> AppURI -> T.Text
renderAppURI config appURI =
  requiredURIScheme config <> "//" <> cfDomain config <> port <> path
  where
    port = maybe "" (":" <>) $ cfPort config
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
    UsersURI -> ["users"]
    UserURI (UserId userId) -> ["users", T.pack $ show userId]
    AuthorsURI -> ["authors"]
    AuthorURI (AuthorId authorId) -> ["authors", T.pack $ show authorId]
    CategoriesURI -> ["categories"]
    CategoryURI (CategoryId catId) -> ["categories", T.pack $ show catId]
    NewsURI -> ["news"]
    NewsItemURI (NewsId newsId) -> ["news", T.pack $ show newsId]
    TagsURI -> ["tags"]
    TagURI (TagId tid) -> ["tags", T.pack $ show tid]
    DraftsURI -> ["drafts"]
    DraftURI (NewsVersionId vid) -> ["drafts", T.pack $ show vid]
    PublishDraftURI (NewsVersionId vid) ->
      ["drafts", T.pack $ show vid, "publish"]
    CommentsURI (NewsId newsId') -> ["news", T.pack $ show newsId', "comments"]
    CommentURI (NewsId newsId') (CommentId commentId') ->
      ["news", T.pack $ show newsId', "comments", T.pack $ show commentId']

parseAppURI :: AppURIConfig -> T.Text -> Maybe AppURI
parseAppURI config uriText = do
  URI.URI {uriScheme, uriAuthority, uriPath} <-
    URI.parseAbsoluteURI $ T.unpack uriText
  URI.URIAuth {uriRegName} <- uriAuthority
  guard $ uriScheme == T.unpack (requiredURIScheme config)
  guard $ uriRegName == T.unpack (cfDomain config)
  fromRelativeURI . RelativeURI . Http.decodePathSegments $ B.pack uriPath

requiredURIScheme :: AppURIConfig -> Text
requiredURIScheme AppURIConfig {cfUseHTTPS}
  | cfUseHTTPS = "https:"
  | otherwise = "http:"

fromRelativeURI :: RelativeURI -> Maybe AppURI
fromRelativeURI (RelativeURI path) =
  case path of
    ["images", id'] -> ImageURI . ImageId <$> readExactIntegral (T.unpack id')
    ["users"] -> Just UsersURI
    ["users", id'] -> UserURI . UserId <$> readExactIntegral (T.unpack id')
    ["authors"] -> Just AuthorsURI
    ["authors", id'] ->
      AuthorURI . AuthorId <$> readExactIntegral (T.unpack id')
    ["categories"] -> Just CategoriesURI
    ["categories", id'] ->
      CategoryURI . CategoryId <$> readExactIntegral (T.unpack id')
    ["news"] -> Just NewsURI
    ["news", id'] -> NewsItemURI . NewsId <$> readExactIntegral (T.unpack id')
    ["news", newsId', "comments"] ->
      CommentsURI . NewsId <$> readExactIntegral (T.unpack newsId')
    ["news", newsId', "comments", commentId'] ->
      liftA2
        CommentURI
        (NewsId <$> readExactIntegral (T.unpack newsId'))
        (CommentId <$> readExactIntegral (T.unpack commentId'))
    ["tags"] -> Just TagsURI
    ["tags", id'] -> TagURI . TagId <$> readExactIntegral (T.unpack id')
    ["drafts"] -> Just DraftsURI
    ["drafts", id'] ->
      DraftURI . NewsVersionId <$> readExactIntegral (T.unpack id')
    ["drafts", id', "publish"] ->
      PublishDraftURI . NewsVersionId <$> readExactIntegral (T.unpack id')
    _ -> Nothing
