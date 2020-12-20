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
import Data.Text.Show
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
  | NewsListURI
  | NewsItemURI NewsId
  | NewsItemDraftsURI NewsId
  | TagsURI
  | TagURI TagId
  | DraftsURI
  | AuthorDraftsURI AuthorId
  | DraftURI DraftId
  | PublishDraftURI DraftId
  | CommentsForNewsURI NewsId
  | CommentURI CommentId
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
    ImageURI (ImageId imageId) -> ["images", showAsText imageId]
    UsersURI -> ["users"]
    UserURI (UserId userId) -> ["users", showAsText userId]
    AuthorsURI -> ["authors"]
    AuthorDraftsURI (AuthorId authorId) ->
      ["authors", showAsText authorId, "drafts"]
    AuthorURI (AuthorId authorId) -> ["authors", showAsText authorId]
    CategoriesURI -> ["categories"]
    CategoryURI (CategoryId catId) -> ["categories", showAsText catId]
    NewsListURI -> ["news"]
    NewsItemURI (NewsId newsId) -> ["news", showAsText newsId]
    NewsItemDraftsURI (NewsId newsId) -> ["news", showAsText newsId, "drafts"]
    TagsURI -> ["tags"]
    TagURI (TagId tid) -> ["tags", showAsText tid]
    DraftsURI -> ["drafts"]
    DraftURI (DraftId draftId) -> ["drafts", showAsText draftId]
    PublishDraftURI (DraftId did) -> ["drafts", showAsText did, "publish"]
    CommentsForNewsURI (NewsId newsId') ->
      ["news", showAsText newsId', "comments"]
    CommentURI (CommentId commentId') -> ["comments", showAsText commentId']

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
    ["authors", id', "drafts"] ->
      AuthorDraftsURI . AuthorId <$> readExactIntegral (T.unpack id')
    ["categories"] -> Just CategoriesURI
    ["categories", id'] ->
      CategoryURI . CategoryId <$> readExactIntegral (T.unpack id')
    ["news"] -> Just NewsListURI
    ["news", id'] -> NewsItemURI . NewsId <$> readExactIntegral (T.unpack id')
    ["news", newsId', "comments"] ->
      CommentsForNewsURI . NewsId <$> readExactIntegral (T.unpack newsId')
    ["news", newsId', "drafts"] ->
      NewsItemDraftsURI . NewsId <$> readExactIntegral (T.unpack newsId')
    ["comments", commentId'] ->
      CommentURI . CommentId <$> readExactIntegral (T.unpack commentId')
    ["tags"] -> Just TagsURI
    ["tags", id'] -> TagURI . TagId <$> readExactIntegral (T.unpack id')
    ["drafts"] -> Just DraftsURI
    ["drafts", id'] -> DraftURI . DraftId <$> readExactIntegral (T.unpack id')
    ["drafts", id', "publish"] ->
      PublishDraftURI . DraftId <$> readExactIntegral (T.unpack id')
    _ -> Nothing
