module Web.Presenter
  -- * Authors
  ( presentCreatedAuthor
  , presentUpdatedAuthor
  , presentDeletedAuthor
  , presentAuthor
  , presentAuthors
  -- * Users
  , presentCreatedUser
  , presentDeletedUser
  , presentUser
  , presentUsers
  -- * Images
  , presentImage
  -- * News and drafts
  , presentNewsList
  , presentNewsItem
  , presentCreatedOrUpdatedNewsItem
  , presentCreatedDraft
  , presentDrafts
  , presentDraft
  , presentDeletedDraft
  -- * Categories
  , presentCreatedCategory
  , presentCategory
  , presentCategories
  , presentDeletedCategory
  , presentUpdatedCategory
  -- * Tags
  , presentCreatedTag
  , presentTag
  , presentTags
  , presentDeletedTag
  , presentUpdatedTag
  -- * Comments
  , presentCreatedComment
  , presentComment
  , presentComments
  , presentDeletedComment
  ) where

import Core.Authentication
import Core.Author
import Core.Category
import Core.Comment
import Core.Image
import Core.Interactor.CreateTag as ICreateTag
import Core.Interactor.PublishDraft as IPublishDraft
import Core.News
import Core.Tag
import Core.User
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as T
import Web.AppURI
import Web.Application
import Web.Representation.Author
import Web.Representation.Category
import Web.Representation.Comment
import Web.Representation.Draft
import Web.Representation.News
import Web.Representation.Tag
import Web.Representation.User
import Web.RepresentationBuilder
import Web.Response

presentCreatedAuthor :: AppURIConfig -> RepBuilderHandle -> Author -> Response
presentCreatedAuthor uriConfig h author =
  resourceCreatedAndReturnedResponse uriConfig (authorURI author) .
  runRepBuilder h $
  authorRep author

presentUpdatedAuthor :: AppURIConfig -> RepBuilderHandle -> Author -> Response
presentUpdatedAuthor uriConfig h author =
  resourceModifiedAndReturnedResponse uriConfig (authorURI author) .
  runRepBuilder h $
  authorRep author

authorURI :: Author -> AppURI
authorURI = AuthorURI . authorId

presentDeletedAuthor :: Response
presentDeletedAuthor = noContentResponse

presentAuthor :: RepBuilderHandle -> Author -> Response
presentAuthor h = dataResponse . runRepBuilder h . authorRep

presentAuthors :: RepBuilderHandle -> [Author] -> Response
presentAuthors h = dataResponse . runRepBuilder h . mapM authorRep

presentCreatedUser ::
     AppURIConfig -> RepBuilderHandle -> User -> Credentials -> Response
presentCreatedUser uriConfig h user creds =
  resourceCreatedAndReturnedResponse uriConfig uri . runRepBuilder h $
  userRep (Just creds) user
  where
    uri = UserURI $ userId user

presentDeletedUser :: Response
presentDeletedUser = noContentResponse

presentUser :: RepBuilderHandle -> User -> Response
presentUser h = dataResponse . runRepBuilder h . userRep Nothing

presentUsers :: RepBuilderHandle -> [User] -> Response
presentUsers h = dataResponse . runRepBuilder h . mapM (userRep Nothing)

presentImage :: Image -> Response
presentImage Image {..} =
  dataResponse
    ResourceRepresentation
      { resourceRepresentationBody = BB.byteString imageData
      , resourceRepresentationContentType =
          contentType $ T.encodeUtf8 imageContentType
      }

presentNewsList :: RepBuilderHandle -> [News] -> Response
presentNewsList h = dataResponse . runRepBuilder h . mapM newsRep

presentNewsItem :: RepBuilderHandle -> News -> Response
presentNewsItem h = dataResponse . runRepBuilder h . newsRep

presentDrafts :: RepBuilderHandle -> [Draft] -> Response
presentDrafts h = dataResponse . runRepBuilder h . mapM draftRep

presentDraft :: RepBuilderHandle -> Draft -> Response
presentDraft h = dataResponse . runRepBuilder h . draftRep

presentDeletedDraft :: Response
presentDeletedDraft = noContentResponse

presentCreatedCategory ::
     AppURIConfig -> RepBuilderHandle -> Category -> Response
presentCreatedCategory uriConfig h category =
  resourceCreatedAndReturnedResponse uriConfig (categoryURI category) .
  runRepBuilder h $
  categoryRep category

presentUpdatedCategory ::
     AppURIConfig -> RepBuilderHandle -> Category -> Response
presentUpdatedCategory uriConfig h category =
  resourceModifiedAndReturnedResponse uriConfig (categoryURI category) .
  runRepBuilder h $
  categoryRep category

presentCategory :: RepBuilderHandle -> Category -> Response
presentCategory h = dataResponse . runRepBuilder h . categoryRep

presentCategories :: RepBuilderHandle -> [Category] -> Response
presentCategories h = dataResponse . runRepBuilder h . mapM categoryRep

categoryURI :: Category -> AppURI
categoryURI cat = CategoryURI $ categoryId cat

presentDeletedCategory :: Response
presentDeletedCategory = noContentResponse

presentCreatedTag ::
     AppURIConfig -> RepBuilderHandle -> ICreateTag.Result -> Response
presentCreatedTag uriConfig h result =
  case result of
    ICreateTag.TagCreated tag ->
      resourceCreatedAndReturnedResponse uriConfig (tagURI tag) .
      runRepBuilder h $
      tagRep tag
    ICreateTag.ExistingTagFound tag ->
      anotherResourceReturnedResponse uriConfig (tagURI tag) . runRepBuilder h $
      tagRep tag

tagURI :: Tag -> AppURI
tagURI = TagURI . tagId

presentTag :: RepBuilderHandle -> Tag -> Response
presentTag h = dataResponse . runRepBuilder h . tagRep

presentTags :: RepBuilderHandle -> [Tag] -> Response
presentTags h = dataResponse . runRepBuilder h . mapM tagRep

presentDeletedTag :: Response
presentDeletedTag = noContentResponse

presentUpdatedTag :: AppURIConfig -> RepBuilderHandle -> Tag -> Response
presentUpdatedTag uriConfig h tag =
  resourceModifiedAndReturnedResponse uriConfig (tagURI tag) . runRepBuilder h $
  tagRep tag

presentCreatedDraft :: AppURIConfig -> RepBuilderHandle -> Draft -> Response
presentCreatedDraft uriConfig h draft =
  resourceCreatedAndReturnedResponse uriConfig (draftURI draft) .
  runRepBuilder h $
  draftRep draft

draftURI :: Draft -> AppURI
draftURI = DraftURI . draftId

presentCreatedOrUpdatedNewsItem ::
     AppURIConfig -> RepBuilderHandle -> IPublishDraft.Success -> Response
presentCreatedOrUpdatedNewsItem uriConfig h IPublishDraft.Success {..} =
  present uriConfig (newsItemURI sNews) (runRepBuilder h $ newsRep sNews)
  where
    present =
      case sStatus of
        IPublishDraft.NewsIsCreated -> resourceCreatedAndReturnedResponse
        IPublishDraft.NewsIsUpdated -> resourceModifiedAndReturnedResponse

newsItemURI :: News -> AppURI
newsItemURI = NewsItemURI . newsId

presentCreatedComment :: AppURIConfig -> RepBuilderHandle -> Comment -> Response
presentCreatedComment uriConfig h comment =
  resourceCreatedAndReturnedResponse uriConfig (commentURI comment) .
  runRepBuilder h $
  commentRep comment

commentURI :: Comment -> AppURI
commentURI Comment {..} = CommentURI commentId

presentComment :: RepBuilderHandle -> Comment -> Response
presentComment h = dataResponse . runRepBuilder h . commentRep

presentComments :: RepBuilderHandle -> [Comment] -> Response
presentComments h = dataResponse . runRepBuilder h . mapM commentRep

presentDeletedComment :: Response
presentDeletedComment = noContentResponse
