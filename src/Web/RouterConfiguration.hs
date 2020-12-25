{-# LANGUAGE DeriveFunctor #-}

module Web.RouterConfiguration
  ( router
  , Handle(..)
  ) where

import Core.Author
import Core.Category
import Core.Comment
import Core.Image
import Core.News
import Core.Tag
import Core.User
import Web.AppURI
import Web.Application
import qualified Web.Router as R

type Handler = Session -> Application

router :: Handle Handler -> R.Router Handler
router Handle {..} =
  R.new $ \case
    ImageURI imageId -> [R.get $ hRunGetImageHandler imageId]
    UsersURI -> [R.get hRunGetUsersHandler, R.post hRunCreateUserHandler]
    UserURI userId ->
      [ R.get $ hRunGetUserHandler userId
      , R.delete $ hRunDeleteUserHandler userId
      ]
    AuthorsURI -> [R.get hRunGetAuthorsHandler, R.post hRunCreateAuthorHandler]
    AuthorURI authorId ->
      [ R.get $ hRunGetAuthorHandler authorId
      , R.delete $ hRunDeleteAuthorHandler authorId
      , R.patch $ hRunPatchAuthorHandler authorId
      ]
    CategoriesURI ->
      [R.get hRunGetCategoriesHandler, R.post hRunCreateCategoryHandler]
    CategoryURI categoryId ->
      [ R.get $ hRunGetCategoryHandler categoryId
      , R.delete $ hRunDeleteCategoryHandler categoryId
      , R.patch $ hRunUpdateCategoryHandler categoryId
      ]
    NewsListURI -> [R.get hRunGetNewsListHandler]
    NewsItemURI newsId -> [R.get $ hRunGetNewsHandler newsId]
    TagsURI -> [R.get hRunGetTagsHandler, R.post hRunCreateTagHandler]
    TagURI tagId ->
      [ R.get $ hRunGetTagHandler tagId
      , R.delete $ hRunDeleteTagHandler tagId
      , R.patch $ hRunPatchTagHandler tagId
      ]
    DraftsURI -> [R.get hRunGetDraftsHandler, R.post hRunCreateDraftHandler]
    AuthorDraftsURI authorId -> [R.get $ hRunGetAuthorDraftsHandler authorId]
    DraftURI draftId ->
      [ R.get $ hRunGetDraftHandler draftId
      , R.delete $ hRunDeleteDraftHandler draftId
      , R.patch $ hRunPatchDraftHandler draftId
      ]
    PublishDraftURI draftId -> [R.post $ hRunPublishDraftHandler draftId]
    CommentsForNewsURI newsId ->
      [ R.get $ hRunGetCommentsForNewsHandler newsId
      , R.post $ hRunCreateCommentHandler newsId
      ]
    CommentURI commentId ->
      [ R.get $ hRunGetCommentHandler commentId
      , R.delete $ hRunDeleteCommentHandler commentId
      ]
    NewsItemDraftsURI newsId ->
      [ R.get $ hRunGetDraftsOfNewsArticleHandler newsId
      , R.post $ hRunCreateDraftFromNewsHandler newsId
      ]

-- | The handle contains external dependencies. It is made a functor
-- over the handler type to simplify handlers management.
data Handle handler =
  Handle
    { hRunGetImageHandler :: ImageId -> handler
    , hRunGetUsersHandler :: handler
    , hRunCreateUserHandler :: handler
    , hRunGetUserHandler :: UserId -> handler
    , hRunDeleteUserHandler :: UserId -> handler
    , hRunGetAuthorsHandler :: handler
    , hRunCreateAuthorHandler :: handler
    , hRunGetAuthorHandler :: AuthorId -> handler
    , hRunDeleteAuthorHandler :: AuthorId -> handler
    , hRunPatchAuthorHandler :: AuthorId -> handler
    , hRunGetCategoriesHandler :: handler
    , hRunCreateCategoryHandler :: handler
    , hRunGetCategoryHandler :: CategoryId -> handler
    , hRunDeleteCategoryHandler :: CategoryId -> handler
    , hRunUpdateCategoryHandler :: CategoryId -> handler
    , hRunGetNewsListHandler :: handler
    , hRunGetNewsHandler :: NewsId -> handler
    , hRunGetTagsHandler :: handler
    , hRunCreateTagHandler :: handler
    , hRunGetTagHandler :: TagId -> handler
    , hRunDeleteTagHandler :: TagId -> handler
    , hRunPatchTagHandler :: TagId -> handler
    , hRunGetDraftsHandler :: handler
    , hRunCreateDraftHandler :: handler
    , hRunGetAuthorDraftsHandler :: AuthorId -> handler
    , hRunGetDraftHandler :: DraftId -> handler
    , hRunDeleteDraftHandler :: DraftId -> handler
    , hRunPatchDraftHandler :: DraftId -> handler
    , hRunPublishDraftHandler :: DraftId -> handler
    , hRunGetCommentsForNewsHandler :: NewsId -> handler
    , hRunCreateCommentHandler :: NewsId -> handler
    , hRunGetCommentHandler :: CommentId -> handler
    , hRunDeleteCommentHandler :: CommentId -> handler
    , hRunGetDraftsOfNewsArticleHandler :: NewsId -> handler
    , hRunCreateDraftFromNewsHandler :: NewsId -> handler
    }
  deriving (Functor)
