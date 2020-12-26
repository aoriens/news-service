{-# LANGUAGE RankNTypes #-}

-- | The module can create and configure 'Web.RouterConfiguration.Handle'.
module Handlers
  ( handlers
  , Handler
  , Deps(..)
  ) where

import qualified Config as Cf
import Control.Monad.IO.Class
import Core.Authentication
import Core.Author
import Core.Category
import Core.Comment
import Core.Image
import Core.ImageValidator
import qualified Core.Interactor.CreateAuthor as ICreateAuthor
import qualified Core.Interactor.CreateCategory as ICreateCategory
import qualified Core.Interactor.CreateComment as ICreateComment
import qualified Core.Interactor.CreateDraft as ICreateDraft
import qualified Core.Interactor.CreateDraftFromNews as ICreateDraftFromNews
import qualified Core.Interactor.CreateTag as ICreateTag
import qualified Core.Interactor.CreateUser as ICreateUser
import qualified Core.Interactor.DeleteAuthor as IDeleteAuthor
import qualified Core.Interactor.DeleteCategory as IDeleteCategory
import qualified Core.Interactor.DeleteComment as IDeleteComment
import qualified Core.Interactor.DeleteDraft as IDeleteDraft
import qualified Core.Interactor.DeleteTag as IDeleteTag
import qualified Core.Interactor.DeleteUser as IDeleteUser
import qualified Core.Interactor.GetAuthor as IGetAuthor
import qualified Core.Interactor.GetAuthors as IGetAuthors
import qualified Core.Interactor.GetCategories as IGetCategories
import qualified Core.Interactor.GetCategory as IGetCategory
import qualified Core.Interactor.GetComment as IGetComment
import qualified Core.Interactor.GetCommentsForNews as IGetCommentsForNews
import qualified Core.Interactor.GetDraft as IGetDraft
import qualified Core.Interactor.GetDrafts as IGetDrafts
import qualified Core.Interactor.GetDraftsOfNewsArticle as IGetDraftsOfNewsArticle
import qualified Core.Interactor.GetImage as IGetImage
import qualified Core.Interactor.GetNews as IGetNews
import qualified Core.Interactor.GetNewsList as IGetNewsList
import qualified Core.Interactor.GetTag as IGetTag
import qualified Core.Interactor.GetTags as IGetTags
import qualified Core.Interactor.GetUser as IGetUser
import qualified Core.Interactor.GetUsers as IGetUsers
import qualified Core.Interactor.PublishDraft as IPublishDraft
import qualified Core.Interactor.UpdateAuthor as IUpdateAuthor
import qualified Core.Interactor.UpdateCategory as IUpdateCategory
import qualified Core.Interactor.UpdateDraft as IUpdateDraft
import qualified Core.Interactor.UpdateTag as IUpdateTag
import Core.News
import Core.Pagination
import Core.Tag
import Core.User
import qualified Data.Aeson as A
import qualified Database
import qualified Database.Service.ConnectionManager as DBConnManager
import qualified Database.Service.Primitives as Database
import Gateway.CurrentTime as GCurrentTime
import qualified Gateway.SecretToken as GSecretToken
import qualified Logger
import Web.AppURI
import qualified Web.Application as Web
import qualified Web.Handler.CreateAuthor as HCreateAuthor
import qualified Web.Handler.CreateCategory as HCreateCategory
import qualified Web.Handler.CreateComment as HCreateComment
import qualified Web.Handler.CreateDraft as HCreateDraft
import qualified Web.Handler.CreateDraftFromNews as HCreateDraftFromNews
import qualified Web.Handler.CreateTag as HCreateTag
import qualified Web.Handler.CreateUser as HCreateUser
import qualified Web.Handler.DeleteAuthor as HDeleteAuthor
import qualified Web.Handler.DeleteCategory as HDeleteCategory
import qualified Web.Handler.DeleteComment as HDeleteComment
import qualified Web.Handler.DeleteDraft as HDeleteDraft
import qualified Web.Handler.DeleteTag as HDeleteTag
import qualified Web.Handler.DeleteUser as HDeleteUser
import qualified Web.Handler.GetAuthor as HGetAuthor
import qualified Web.Handler.GetAuthors as HGetAuthors
import qualified Web.Handler.GetCategories as HGetCategories
import qualified Web.Handler.GetCategory as HGetCategory
import qualified Web.Handler.GetComment as HGetComment
import qualified Web.Handler.GetCommentsForNews as HGetCommentsForNews
import qualified Web.Handler.GetDraft as HGetDraft
import qualified Web.Handler.GetDrafts as HGetDrafts
import qualified Web.Handler.GetDraftsOfNewsArticle as HGetDraftsOfNewsArticle
import qualified Web.Handler.GetImage as HGetImage
import qualified Web.Handler.GetNews as HGetNews
import qualified Web.Handler.GetNewsList as HGetNewsList
import qualified Web.Handler.GetTag as HGetTag
import qualified Web.Handler.GetTags as HGetTags
import qualified Web.Handler.GetUser as HGetUser
import qualified Web.Handler.GetUsers as HGetUsers
import qualified Web.Handler.PatchAuthor as HPatchAuthor
import qualified Web.Handler.PatchCategory as HPatchCategory
import qualified Web.Handler.PatchDraft as HPatchDraft
import qualified Web.Handler.PatchTag as HPatchTag
import qualified Web.Handler.PublishDraft as HPublishDraft
import Web.Presenter
import Web.RepresentationBuilder
import qualified Web.RouterConfiguration

-- | External dependencies we need to have passed in.
data Deps =
  Deps
    { dDatabaseConnectionConfig :: DBConnManager.Config
    , dConfig :: Cf.Config
    , dLoggerHandleWith :: Web.Session -> Logger.Handle IO
    , dPageSpecParserHandle :: PageSpecParserHandle
    , dLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Web.Request -> Database.Transaction a
    , dSecretTokenIOState :: GSecretToken.IOState
    , dAppURIConfig :: AppURIConfig
    , dRepresentationBuilderHandle :: RepBuilderHandle
    , dDatabaseHandle :: Database.Handle
    , dAuthenticate :: Maybe Credentials -> Database.Transaction AuthenticatedUser
    }

type Handler = Deps -> Web.GenericApplication Database.Transaction

handlers :: Web.RouterConfiguration.Handlers Handler
handlers =
  Web.RouterConfiguration.Handlers
    { hRunGetImageHandler = runGetImageHandler
    , hRunGetUsersHandler = runGetUsersHandler
    , hRunCreateUserHandler = runCreateUserHandler
    , hRunGetUserHandler = runGetUserHandler
    , hRunDeleteUserHandler = runDeleteUserHandler
    , hRunGetAuthorsHandler = runGetAuthorsHandler
    , hRunCreateAuthorHandler = runCreateAuthorHandler
    , hRunGetAuthorHandler = runGetAuthorHandler
    , hRunDeleteAuthorHandler = runDeleteAuthorHandler
    , hRunPatchAuthorHandler = runPatchAuthorHandler
    , hRunGetCategoriesHandler = runGetCategoriesHandler
    , hRunCreateCategoryHandler = runCreateCategoryHandler
    , hRunGetCategoryHandler = runGetCategoryHandler
    , hRunDeleteCategoryHandler = runDeleteCategoryHandler
    , hRunUpdateCategoryHandler = runUpdateCategoryHandler
    , hRunGetNewsListHandler = runGetNewsListHandler
    , hRunGetNewsHandler = runGetNewsHandler
    , hRunGetTagsHandler = runGetTagsHandler
    , hRunCreateTagHandler = runCreateTagHandler
    , hRunGetTagHandler = runGetTagHandler
    , hRunDeleteTagHandler = runDeleteTagHandler
    , hRunPatchTagHandler = runPatchTagHandler
    , hRunGetDraftsHandler = runGetDraftsHandler Nothing
    , hRunCreateDraftHandler = runCreateDraftHandler
    , hRunGetAuthorDraftsHandler = runGetDraftsHandler . Just
    , hRunGetDraftHandler = runGetDraftHandler
    , hRunDeleteDraftHandler = runDeleteDraftHandler
    , hRunPatchDraftHandler = runPatchDraftHandler
    , hRunPublishDraftHandler = runPublishDraftHandler
    , hRunGetCommentsForNewsHandler = runGetCommentsForNewsHandler
    , hRunCreateCommentHandler = runCreateCommentHandler
    , hRunGetCommentHandler = runGetCommentHandler
    , hRunDeleteCommentHandler = runDeleteCommentHandler
    , hRunGetDraftsOfNewsArticleHandler = runGetDraftsOfNewsArticleHandler
    , hRunCreateDraftFromNewsHandler = runCreateDraftFromNewsHandler
    }

runCreateAuthorHandler :: Handler
runCreateAuthorHandler Deps {..} =
  HCreateAuthor.run
    HCreateAuthor.Handle
      { hCreateAuthor = ICreateAuthor.run interactorH
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent =
          presentCreatedAuthor dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      }
  where
    interactorH = ICreateAuthor.Handle {hCreateAuthor = Database.createAuthor}

runGetAuthorsHandler :: Handler
runGetAuthorsHandler Deps {..} =
  HGetAuthors.run
    HGetAuthors.Handle
      { hGetAuthors = IGetAuthors.run interactorH
      , hPresent = presentAuthors dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      }
  where
    interactorH =
      IGetAuthors.Handle
        { hGetAuthors = Database.getAuthors
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runPatchAuthorHandler :: AuthorId -> Handler
runPatchAuthorHandler authorId Deps {..} =
  HPatchAuthor.run
    HPatchAuthor.Handle
      { hUpdateAuthor = IUpdateAuthor.run interactorH
      , hPresent =
          presentUpdatedAuthor dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hAuthenticate = dAuthenticate
      }
    authorId
  where
    interactorH = IUpdateAuthor.Handle {hUpdateAuthor = Database.updateAuthor}

runGetAuthorHandler :: AuthorId -> Handler
runGetAuthorHandler authorId Deps {..} =
  HGetAuthor.run
    HGetAuthor.Handle
      { hGetAuthor = IGetAuthor.run interactorH
      , hPresent = presentAuthor dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      }
    authorId
  where
    interactorH = IGetAuthor.Handle {hGetAuthor = Database.getAuthor}

runDeleteAuthorHandler :: AuthorId -> Handler
runDeleteAuthorHandler authorId Deps {..} =
  HDeleteAuthor.run
    HDeleteAuthor.Handle
      { hDeleteAuthor = IDeleteAuthor.run interactorH
      , hPresent = presentDeletedAuthor
      , hAuthenticate = dAuthenticate
      }
    authorId
  where
    interactorH =
      IDeleteAuthor.Handle
        { hDeleteAuthor = Database.deleteAuthor
        , hDeleteDraftsOfAuthor = Database.deleteDraftsOfAuthor
        }

runCreateCategoryHandler :: Handler
runCreateCategoryHandler Deps {..} =
  HCreateCategory.run
    HCreateCategory.Handle
      { hCreateCategory = ICreateCategory.run interactorH
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent =
          presentCreatedCategory dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      }
  where
    interactorH =
      ICreateCategory.Handle
        { hCreateCategory = Database.createCategory
        , hCategoryIdWithParentAndNameExists =
            Database.categoryIdWithParentAndNameExists
        }

runGetCategoryHandler :: CategoryId -> Handler
runGetCategoryHandler categoryId Deps {..} =
  HGetCategory.run
    HGetCategory.Handle
      { hGetCategory = IGetCategory.run interactorH
      , hPresent = presentCategory dRepresentationBuilderHandle
      }
    categoryId
  where
    interactorH = IGetCategory.Handle {hGetCategory = Database.getCategory}

runGetCategoriesHandler :: Handler
runGetCategoriesHandler Deps {..} =
  HGetCategories.run
    HGetCategories.Handle
      { hGetCategories = IGetCategories.run interactorH
      , hPresent = presentCategories dRepresentationBuilderHandle
      }
  where
    interactorH =
      IGetCategories.Handle
        { hGetCategories = Database.getCategories
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runDeleteCategoryHandler :: CategoryId -> Handler
runDeleteCategoryHandler categoryId Deps {..} =
  HDeleteCategory.run
    HDeleteCategory.Handle
      { hDeleteCategory = IDeleteCategory.run interactorH
      , hPresent = presentDeletedCategory
      , hAuthenticate = dAuthenticate
      }
    categoryId
  where
    interactorH =
      IDeleteCategory.Handle
        { hGetCategory = Database.getCategory
        , hSetCategoryIdToNewsVersionsInCategoryAndDescendantCategories =
            Database.setCategoryIdToNewsVersionsInCategoryAndDescendantCategories
        , hDeleteCategoryAndDescendants = Database.deleteCategoryAndDescendants
        }

runUpdateCategoryHandler :: CategoryId -> Handler
runUpdateCategoryHandler categoryId Deps {..} =
  HPatchCategory.run
    HPatchCategory.Handle
      { hUpdateCategory = IUpdateCategory.run interactorH
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent =
          presentUpdatedCategory dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      }
    categoryId
  where
    interactorH =
      IUpdateCategory.Handle
        { hUpdateCategory = Database.updateCategory
        , hGetCategoryIdBySiblingAndName =
            Database.getCategoryIdBySiblingAndName
        , hGetCategoryIdByParentAndName = Database.getCategoryIdByParentAndName
        , hCategoryIsDescendantOf = Database.categoryIsDescendantOf
        , hGetCategoryName = Database.getCategoryName
        }

runGetNewsListHandler :: Handler
runGetNewsListHandler Deps {..} =
  HGetNewsList.run
    HGetNewsList.Handle
      { hGetNews = IGetNewsList.run interactorH
      , hPresent = presentNewsList dRepresentationBuilderHandle
      }
  where
    interactorH =
      IGetNewsList.Handle
        { hGetNews = Database.getNewsList
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runGetNewsHandler :: NewsId -> Handler
runGetNewsHandler newsId Deps {..} =
  HGetNews.run
    HGetNews.Handle
      { hGetNews = IGetNews.run interactorH
      , hPresent = presentNewsItem dRepresentationBuilderHandle
      }
    newsId
  where
    interactorH = IGetNews.Handle {hGetNews = Database.getNews}

runCreateUserHandler :: Handler
runCreateUserHandler Deps {..} =
  HCreateUser.run
    HCreateUser.Handle
      { hCreateUser = ICreateUser.run interactorH
      , hPresent = presentCreatedUser dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      }
  where
    interactorH =
      ICreateUser.Handle
        { hCreateUser = Database.createUser
        , hGenerateToken =
            liftIO $
            GSecretToken.generateIO secretTokenConfig dSecretTokenIOState
        , hGetCurrentTime = liftIO GCurrentTime.getIntegralSecondsTime
        , hRejectImageIfDisallowed =
            rejectDisallowedImage $ Cf.cfAllowedImageMimeTypes dConfig
        }
    secretTokenConfig =
      GSecretToken.Config {cfTokenLength = Cf.cfSecretTokenLength dConfig}

runGetImageHandler :: ImageId -> Handler
runGetImageHandler imageId Deps {..} =
  HGetImage.run
    HGetImage.Handle
      { hGetImage = IGetImage.run $ IGetImage.Handle Database.getImage
      , hPresent = presentImage
      }
    imageId

runGetUserHandler :: UserId -> Handler
runGetUserHandler userId Deps {..} =
  HGetUser.run
    HGetUser.Handle
      { hGetUser = IGetUser.run interactorH
      , hPresent = presentUser dRepresentationBuilderHandle
      }
    userId
  where
    interactorH = IGetUser.Handle Database.getExistingUser

runDeleteUserHandler :: UserId -> Handler
runDeleteUserHandler userId Deps {..} =
  HDeleteUser.run
    HDeleteUser.Handle
      { hDeleteUser = IDeleteUser.run interactorH
      , hPresent = presentDeletedUser
      , hAuthenticate = dAuthenticate
      }
    userId
  where
    interactorH = IDeleteUser.Handle Database.deleteUser

runGetUsersHandler :: Handler
runGetUsersHandler Deps {..} =
  HGetUsers.run
    HGetUsers.Handle
      { hGetUsers = IGetUsers.run interactorH
      , hPresent = presentUsers dRepresentationBuilderHandle
      }
  where
    interactorH =
      IGetUsers.Handle
        { hGetUsers = Database.getUsers
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runCreateTagHandler :: Handler
runCreateTagHandler Deps {..} =
  HCreateTag.run
    HCreateTag.Handle
      { hCreateTag = ICreateTag.run interactorH
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent = presentCreatedTag dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      }
  where
    interactorH =
      ICreateTag.Handle
        { hCreateTagNamed = Database.createTagNamed
        , hFindTagNamed = Database.findTagNamed
        }

runGetTagHandler :: TagId -> Handler
runGetTagHandler tagId Deps {..} =
  HGetTag.run
    HGetTag.Handle
      { hGetTag = IGetTag.run interactorH
      , hPresent = presentTag dRepresentationBuilderHandle
      }
    tagId
  where
    interactorH = IGetTag.Handle Database.getTag

runDeleteTagHandler :: TagId -> Handler
runDeleteTagHandler tagId Deps {..} =
  HDeleteTag.run
    HDeleteTag.Handle
      { hDeleteTag = IDeleteTag.run interactorH
      , hAuthenticate = dAuthenticate
      , hPresent = presentDeletedTag
      }
    tagId
  where
    interactorH = IDeleteTag.Handle Database.deleteTag

runPatchTagHandler :: TagId -> Handler
runPatchTagHandler tagId' Deps {..} =
  HPatchTag.run
    HPatchTag.Handle
      { hUpdateTag = IUpdateTag.run interactorH
      , hAuthenticate = dAuthenticate
      , hPresent = presentUpdatedTag dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      }
    tagId'
  where
    interactorH =
      IUpdateTag.Handle
        { hFindTagNamed = fmap (fmap tagId) . Database.findTagNamed
        , hSetTagName = Database.setTagName
        }

runGetTagsHandler :: Handler
runGetTagsHandler Deps {..} =
  HGetTags.run
    HGetTags.Handle
      { hGetTags = IGetTags.run interactorH
      , hPresent = presentTags dRepresentationBuilderHandle
      }
  where
    interactorH =
      IGetTags.Handle
        { hGetTags = Database.getTags
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runCreateDraftHandler :: Handler
runCreateDraftHandler Deps {..} =
  HCreateDraft.run
    HCreateDraft.Handle
      { hCreateDraft = ICreateDraft.run interactorH
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent =
          presentCreatedDraft dAppURIConfig dRepresentationBuilderHandle
      , hParseAppURI = Web.AppURI.parseAppURI dAppURIConfig
      , hAuthenticate = dAuthenticate
      }
  where
    interactorH =
      ICreateDraft.Handle
        { hGetAuthorIdByUserIdIfExactlyOne =
            Database.getAuthorIdByUserIdIfExactlyOne
        , hCreateDraft = Database.createDraft
        , hRejectImageIfDisallowed =
            rejectDisallowedImage $ Cf.cfAllowedImageMimeTypes dConfig
        }

runCreateDraftFromNewsHandler :: NewsId -> Handler
runCreateDraftFromNewsHandler newsId Deps {..} =
  HCreateDraftFromNews.run
    HCreateDraftFromNews.Handle
      { hCreateDraftFromNews = ICreateDraftFromNews.run interactorH
      , hPresent =
          presentCreatedDraft dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      }
    newsId
  where
    interactorH =
      ICreateDraftFromNews.Handle
        { hGetNewsAuthorId = Database.getNewsAuthorId
        , hCopyDraftFromNews = Database.copyDraftFromNews
        }

runGetDraftsOfNewsArticleHandler :: NewsId -> Handler
runGetDraftsOfNewsArticleHandler newsId Deps {..} =
  HGetDraftsOfNewsArticle.run
    HGetDraftsOfNewsArticle.Handle
      { hGetDraftsOfNewsArticle = IGetDraftsOfNewsArticle.run interactorH
      , hPresent = presentDrafts dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      , hParsePageSpec = parsePageSpecM dPageSpecParserHandle
      }
    newsId
  where
    interactorH =
      IGetDraftsOfNewsArticle.Handle
        { hGetNewsAuthorId = Database.getNewsAuthorId
        , hGetDraftsCreatedFromNewsId = Database.getDraftsCreatedFromNewsId
        }

runPublishDraftHandler :: DraftId -> Handler
runPublishDraftHandler draftId Deps {..} =
  HPublishDraft.run
    HPublishDraft.Handle
      { hPublishDraft = IPublishDraft.run interactorH
      , hPresent =
          presentCreatedOrUpdatedNewsItem
            dAppURIConfig
            dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      }
    draftId
  where
    interactorH =
      IPublishDraft.Handle
        { hGetDraftAuthorAndNewsIdItWasCreatedFrom =
            Database.getDraftAuthorAndNewsIdItWasCreatedFrom
        , hGetCurrentDay = liftIO getCurrentDay
        , hMakeDraftIntoNews = Database.makeDraftIntoNews
        , hOverwriteNewsWithDraft = Database.overwriteNewsWithDraft
        }

runGetDraftsHandler :: Maybe AuthorId -> Handler
runGetDraftsHandler optAuthorId Deps {..} =
  HGetDrafts.run
    HGetDrafts.Handle
      { hGetDrafts = IGetDrafts.run interactorH
      , hAuthenticate = dAuthenticate
      , hPresent = presentDrafts dRepresentationBuilderHandle
      }
    optAuthorId
  where
    interactorH =
      IGetDrafts.Handle
        { hGetDraftsOfAuthor = Database.getDraftsOfAuthor
        , hGetDraftsOfUser = Database.getDraftsOfUser
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runGetDraftHandler :: DraftId -> Handler
runGetDraftHandler draftId Deps {..} =
  HGetDraft.run
    HGetDraft.Handle
      { hGetDraft = IGetDraft.run interactorH
      , hAuthenticate = dAuthenticate
      , hPresent = presentDraft dRepresentationBuilderHandle
      }
    draftId
  where
    interactorH = IGetDraft.Handle Database.getDraft

runDeleteDraftHandler :: DraftId -> Handler
runDeleteDraftHandler draftId Deps {..} =
  HDeleteDraft.run
    HDeleteDraft.Handle
      { hDeleteDraft = IDeleteDraft.run interactorH
      , hAuthenticate = dAuthenticate
      , hPresent = presentDeletedDraft
      }
    draftId
  where
    interactorH =
      IDeleteDraft.Handle
        { hGetDraftAuthor = Database.getDraftAuthor
        , hDeleteDraftAndItsContent = Database.deleteDraftAndItsContent
        }

runPatchDraftHandler :: DraftId -> Handler
runPatchDraftHandler draftId Deps {..} =
  HPatchDraft.run
    HPatchDraft.Handle
      { hUpdateDraft = IUpdateDraft.run interactorH
      , hAuthenticate = dAuthenticate
      , hPresent =
          presentUpdatedDraft dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hParseAppURI = Web.AppURI.parseAppURI dAppURIConfig
      }
    draftId
  where
    interactorH =
      IUpdateDraft.Handle
        { hGetDraftAuthor = Database.getDraftAuthor
        , hUpdateDraft = Database.updateDraft
        }

runCreateCommentHandler :: NewsId -> Handler
runCreateCommentHandler newsId Deps {..} =
  HCreateComment.run
    HCreateComment.Handle
      { hCreateComment = ICreateComment.run interactorH
      , hPresent =
          presentCreatedComment dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = dAuthenticate
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      }
    newsId
  where
    interactorH =
      ICreateComment.Handle
        { hCreateComment = Database.createComment
        , hGetCurrentTime = liftIO GCurrentTime.getIntegralSecondsTime
        }

runGetCommentHandler :: CommentId -> Handler
runGetCommentHandler commentId Deps {..} =
  HGetComment.run
    HGetComment.Handle
      { hGetComment = IGetComment.run interactorH
      , hPresent = presentComment dRepresentationBuilderHandle
      }
    commentId
  where
    interactorH = IGetComment.Handle {hGetComment = Database.getComment}

runDeleteCommentHandler :: CommentId -> Handler
runDeleteCommentHandler commentId Deps {..} =
  HDeleteComment.run
    HDeleteComment.Handle
      { hDeleteComment = IDeleteComment.run interactorH
      , hAuthenticate = dAuthenticate
      , hPresent = presentDeletedComment
      }
    commentId
  where
    interactorH =
      IDeleteComment.Handle
        { hDeleteComment = Database.deleteComment
        , hGetCommentAuthor = Database.getCommentAuthor
        }

runGetCommentsForNewsHandler :: NewsId -> Handler
runGetCommentsForNewsHandler newsId Deps {..} =
  HGetCommentsForNews.run
    HGetCommentsForNews.Handle
      { hGetCommentsForNews = IGetCommentsForNews.run interactorH
      , hPresent = presentComments dRepresentationBuilderHandle
      }
    newsId
  where
    interactorH =
      IGetCommentsForNews.Handle
        { hGetCommentsForNews = Database.getCommentsForNews
        , hPageSpecParserHandle = dPageSpecParserHandle
        }
