{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  ) where

import qualified Config as Cf
import qualified Config.IO as CIO
import Control.Concurrent.Async
import Control.Exception
import Control.Exception.Sync
import Control.Monad.IO.Class
import Core.Authentication
import Core.Authentication.Impl as AuthImpl
import Core.Author
import qualified Core.Authorization.Impl
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
import qualified Core.Pagination.Impl
import Core.Tag
import Core.User
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import Data.Text.Show
import qualified Database
import qualified Database.Service.ConnectionManager as DBConnManager
import qualified Database.Service.Primitives as Database
import qualified FrontEnd.Wai
import Gateway.CurrentTime as GCurrentTime
import qualified Gateway.SecretToken as GSecretToken
import qualified Logger
import qualified Logger.Impl
import qualified Network.Wai.Handler.Warp as Warp
import System.Exit
import System.IO hiding (Handle)
import Web.AppURI
import qualified Web.Application as Web
import qualified Web.EntryPoint
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
import qualified Web.JSONEncoder as JSONEncoder
import Web.Presenter
import Web.Presenter.Error
import Web.RepresentationBuilder
import qualified Web.RequestBodyLoader as RequestBodyLoader
import qualified Web.Router as R

-- Some common module dependencies. Its purpose is to be passed to
-- functions **in this module**, keeping extensibility in the number
-- of fields and avoiding to add excess parameters to 100500 function
-- signatures.
data Deps =
  Deps
    { dDatabaseConnectionConfig :: DBConnManager.Config
    , dConfig :: Cf.Config
    , dLoggerHandle :: Logger.Handle IO
    , dPageSpecParserHandle :: PageSpecParserHandle
    , dJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , dLoadJSONRequestBody' :: forall a. A.FromJSON a =>
                                           Web.Request -> Database.Transaction a
    , dLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Web.Request -> IO a
    -- ^ @todo: obsolete, should be deleted
    , dSecretTokenIOState :: GSecretToken.IOState
    , dAppURIConfig :: AppURIConfig
    , dRenderAppURI :: AppURI -> T.Text
    , dRepresentationBuilderHandle :: RepBuilderHandle
    , dMakeAuthenticationHandle :: Web.Session -> AuthenticationHandle IO
    }

main :: IO ()
main = do
  (loggerWorker, deps@Deps {..}) <- getDeps
  webHandle <- getWebAppHandle deps
  race_ loggerWorker $ do
    Logger.info dLoggerHandle "Starting Warp"
    Warp.runSettings
      (Cf.cfWarpSettings dConfig)
      (FrontEnd.Wai.toWaiApplication $ Web.EntryPoint.application webHandle)

getDeps :: IO (Logger.Impl.Worker, Deps)
getDeps = do
  inConfig <- CIO.getConfig
  dConfig <- either die pure $ Cf.makeConfig inConfig
  (loggerWorker, dLoggerHandle) <- getLoggerHandle dConfig
  dSecretTokenIOState <- GSecretToken.initIOState
  let dDatabaseConnectionConfig = Cf.cfDatabaseConfig dConfig
      dJSONEncode a =
        JSONEncoder.encode
          JSONEncoder.Config {prettyPrint = Cf.cfJSONPrettyPrint dConfig}
          a
      dAppURIConfig = Cf.cfAppURIConfig dConfig
      dRenderAppURI = Web.AppURI.renderAppURI dAppURIConfig
  pure
    ( loggerWorker
    , Deps
        { dConfig
        , dLoggerHandle
        , dDatabaseConnectionConfig
        , dPageSpecParserHandle =
            Core.Pagination.Impl.new $ Cf.cfMaxPageLimit dConfig
        , dJSONEncode
        , dLoadJSONRequestBody =
            RequestBodyLoader.loadJSONRequestBody
              RequestBodyLoader.Config
                {cfMaxBodySize = Cf.cfMaxRequestJsonBodySize dConfig}
        , dLoadJSONRequestBody' =
            liftIO .
            RequestBodyLoader.loadJSONRequestBody
              RequestBodyLoader.Config
                {cfMaxBodySize = Cf.cfMaxRequestJsonBodySize dConfig}
        , dSecretTokenIOState
        , dAppURIConfig
        , dRenderAppURI
        , dRepresentationBuilderHandle =
            RepBuilderHandle
              {hJSONEncode = dJSONEncode, hRenderAppURI = dRenderAppURI}
        , dMakeAuthenticationHandle =
            \session ->
              AuthImpl.new
                AuthImpl.Handle
                  { hGetUserAuthData =
                      Database.getUserAuthData $
                      sessionDatabaseHandle
                        dDatabaseConnectionConfig
                        dLoggerHandle
                        session
                  , hTokenMatchesHash = GSecretToken.tokenMatchesHash
                  , hLoggerHandle = sessionLoggerHandle session dLoggerHandle
                  }
        })

getWebAppHandle :: Deps -> IO Web.EntryPoint.Handle
getWebAppHandle deps@Deps {..} = do
  hState <- Web.EntryPoint.makeState
  pure
    Web.EntryPoint.Handle
      { hState
      , hLogger = (`sessionLoggerHandle` dLoggerHandle)
      , hRouter = router deps
      , hShowInternalExceptionInfoInResponses =
          Cf.cfShowInternalErrorInfoInResponse dConfig
      , hPresentCoreException =
          presentCoreException dRepresentationBuilderHandle
      , hPresentWebException = presentWebException dRepresentationBuilderHandle
      , hNotFoundResponse = notFoundResponse
      , hMethodNotAllowedResponse = methodNotAllowedResponse
      , hUncaughtExceptionResponseForDebug = uncaughtExceptionResponseForDebug
      }

router :: Deps -> R.Router (Web.Session -> Web.Application)
router deps =
  fmap (\h -> h deps . sessionDeps deps) $
  R.new $ \case
    ImageURI imageId -> [R.get $ runGetImageHandler imageId]
    UsersURI -> [R.get runGetUsersHandler, R.post runCreateUserHandler]
    UserURI userId ->
      [R.get $ runGetUserHandler userId, R.delete $ runDeleteUserHandler userId]
    AuthorsURI -> [R.get runGetAuthorsHandler, R.post runCreateAuthorHandler]
    AuthorURI authorId ->
      [ R.get $ runGetAuthorHandler authorId
      , R.delete $ runDeleteAuthorHandler authorId
      , R.patch $ runPatchAuthorHandler authorId
      ]
    CategoriesURI ->
      [R.get runGetCategoriesHandler, R.post runCreateCategoryHandler]
    CategoryURI categoryId ->
      [ R.get $ runGetCategoryHandler categoryId
      , R.delete $ runDeleteCategoryHandler categoryId
      , R.patch $ runUpdateCategoryHandler categoryId
      ]
    NewsListURI -> [R.get runGetNewsListHandler]
    NewsItemURI newsId -> [R.get $ runGetNewsHandler newsId]
    TagsURI -> [R.get runGetTagsHandler, R.post runCreateTagHandler]
    TagURI tagId ->
      [ R.get $ runGetTagHandler tagId
      , R.delete $ runDeleteTagHandler tagId
      , R.patch $ runPatchTagHandler tagId
      ]
    DraftsURI ->
      [R.get $ runGetDraftsHandler Nothing, R.post runCreateDraftHandler]
    AuthorDraftsURI authorId -> [R.get $ runGetDraftsHandler (Just authorId)]
    DraftURI draftId ->
      [ R.get $ runGetDraftHandler draftId
      , R.delete $ runDeleteDraftHandler draftId
      , R.patch $ runPatchDraftHandler draftId
      ]
    PublishDraftURI draftId -> [R.post $ runPublishDraftHandler draftId]
    CommentsForNewsURI newsId ->
      [ R.get $ runGetCommentsForNewsHandler newsId
      , R.post $ runCreateCommentHandler newsId
      ]
    CommentURI commentId ->
      [ R.get $ runGetCommentHandler commentId
      , R.delete $ runDeleteCommentHandler commentId
      ]
    NewsItemDraftsURI newsId ->
      [ R.get $ runGetDraftsOfNewsArticleHandler newsId
      , R.post $ runCreateDraftFromNewsHandler newsId
      ]

runCreateAuthorHandler :: Deps -> SessionDeps -> Web.Application
runCreateAuthorHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HCreateAuthor.run
    HCreateAuthor.Handle
      { hCreateAuthor = ICreateAuthor.run createAuthorH
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      , hPresent =
          presentCreatedAuthor dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      }
  where
    createAuthorH =
      ICreateAuthor.Handle
        { hAuthorizationHandle = Core.Authorization.Impl.new
        , hCreateAuthor = Database.createAuthor
        }

runGetAuthorsHandler :: Deps -> SessionDeps -> Web.Application
runGetAuthorsHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetAuthors.run
    HGetAuthors.Handle
      { hGetAuthors = IGetAuthors.run getAuthorsH
      , hPresent = presentAuthors dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      }
  where
    getAuthorsH =
      IGetAuthors.Handle
        { hAuthorizationHandle = Core.Authorization.Impl.new
        , hGetAuthors = Database.getAuthors
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runPatchAuthorHandler :: AuthorId -> Deps -> SessionDeps -> Web.Application
runPatchAuthorHandler authorId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HPatchAuthor.run
    HPatchAuthor.Handle
      { hUpdateAuthor = IUpdateAuthor.run updateAuthorH
      , hPresent =
          presentUpdatedAuthor dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      , hAuthenticate = sdAuthenticate
      }
    authorId
  where
    updateAuthorH =
      IUpdateAuthor.Handle
        { hAuthorizationHandle = Core.Authorization.Impl.new
        , hUpdateAuthor = Database.updateAuthor
        }

runGetAuthorHandler :: AuthorId -> Deps -> SessionDeps -> Web.Application
runGetAuthorHandler authorId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetAuthor.run
    HGetAuthor.Handle
      { hGetAuthor = IGetAuthor.run getAuthorH
      , hPresent = presentAuthor dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      }
    authorId
  where
    getAuthorH = IGetAuthor.Handle {hGetAuthor = Database.getAuthor}

runDeleteAuthorHandler :: AuthorId -> Deps -> SessionDeps -> Web.Application
runDeleteAuthorHandler authorId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HDeleteAuthor.run
    HDeleteAuthor.Handle
      { hDeleteAuthor = IDeleteAuthor.run deleteAuthorH
      , hPresent = presentDeletedAuthor
      , hAuthenticate = sdAuthenticate
      }
    authorId
  where
    deleteAuthorH =
      IDeleteAuthor.Handle
        { hDeleteAuthor = Database.deleteAuthor
        , hDeleteDraftsOfAuthor = Database.deleteDraftsOfAuthor
        }

runCreateCategoryHandler :: Deps -> SessionDeps -> Web.Application
runCreateCategoryHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HCreateCategory.run
    HCreateCategory.Handle
      { hCreateCategory = ICreateCategory.run createCategoryH
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      , hPresent =
          presentCreatedCategory dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      }
  where
    createCategoryH =
      ICreateCategory.Handle
        { hCreateCategory = Database.createCategory
        , hCategoryIdWithParentAndNameExists =
            Database.categoryIdWithParentAndNameExists
        }

runGetCategoryHandler :: CategoryId -> Deps -> SessionDeps -> Web.Application
runGetCategoryHandler categoryId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetCategory.run
    HGetCategory.Handle
      { hGetCategory = IGetCategory.run getCategoryH
      , hPresent = presentCategory dRepresentationBuilderHandle
      }
    categoryId
  where
    getCategoryH = IGetCategory.Handle {hGetCategory = Database.getCategory}

runGetCategoriesHandler :: Deps -> SessionDeps -> Web.Application
runGetCategoriesHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetCategories.run
    HGetCategories.Handle
      { hGetCategories = IGetCategories.run getCategoriesH
      , hPresent = presentCategories dRepresentationBuilderHandle
      }
  where
    getCategoriesH =
      IGetCategories.Handle
        { hGetCategories = Database.getCategories
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runDeleteCategoryHandler :: CategoryId -> Deps -> SessionDeps -> Web.Application
runDeleteCategoryHandler categoryId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HDeleteCategory.run
    HDeleteCategory.Handle
      { hDeleteCategory = IDeleteCategory.run deleteCategoryH
      , hPresent = presentDeletedCategory
      , hAuthenticate = sdAuthenticate
      }
    categoryId
  where
    deleteCategoryH =
      IDeleteCategory.Handle
        { hGetCategory = Database.getCategory
        , hSetCategoryIdToNewsVersionsInCategoryAndDescendantCategories =
            Database.setCategoryIdToNewsVersionsInCategoryAndDescendantCategories
        , hDeleteCategoryAndDescendants = Database.deleteCategoryAndDescendants
        }

runUpdateCategoryHandler :: CategoryId -> Deps -> SessionDeps -> Web.Application
runUpdateCategoryHandler categoryId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HPatchCategory.run
    HPatchCategory.Handle
      { hUpdateCategory = IUpdateCategory.run updateCategoryH
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      , hPresent =
          presentUpdatedCategory dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      }
    categoryId
  where
    updateCategoryH =
      IUpdateCategory.Handle
        { hUpdateCategory = Database.updateCategory
        , hGetCategoryIdBySiblingAndName =
            Database.getCategoryIdBySiblingAndName
        , hGetCategoryIdByParentAndName = Database.getCategoryIdByParentAndName
        , hCategoryIsDescendantOf = Database.categoryIsDescendantOf
        , hGetCategoryName = Database.getCategoryName
        }

runGetNewsListHandler :: Deps -> SessionDeps -> Web.Application
runGetNewsListHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetNewsList.run
    HGetNewsList.Handle
      { hGetNews = IGetNewsList.run listNewsH
      , hPresent = presentNewsList dRepresentationBuilderHandle
      }
  where
    listNewsH =
      IGetNewsList.Handle
        { hGetNews = Database.getNewsList
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runGetNewsHandler :: NewsId -> Deps -> SessionDeps -> Web.Application
runGetNewsHandler newsId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetNews.run
    HGetNews.Handle
      { hGetNews = IGetNews.run getNewsH
      , hPresent = presentNewsItem dRepresentationBuilderHandle
      }
    newsId
  where
    getNewsH = IGetNews.Handle {hGetNews = Database.getNews}

runCreateUserHandler :: Deps -> SessionDeps -> Web.Application
runCreateUserHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HCreateUser.run
    HCreateUser.Handle
      { hCreateUser = ICreateUser.run createUserH
      , hPresent = presentCreatedUser dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      }
  where
    createUserH =
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

runGetImageHandler :: ImageId -> Deps -> SessionDeps -> Web.Application
runGetImageHandler imageId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetImage.run
    HGetImage.Handle
      { hGetImage = IGetImage.run $ IGetImage.Handle Database.getImage
      , hPresent = presentImage
      }
    imageId

runGetUserHandler :: UserId -> Deps -> SessionDeps -> Web.Application
runGetUserHandler userId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetUser.run
    HGetUser.Handle
      { hGetUser = IGetUser.run getUserH
      , hPresent = presentUser dRepresentationBuilderHandle
      }
    userId
  where
    getUserH = IGetUser.Handle Database.getExistingUser

runDeleteUserHandler :: UserId -> Deps -> SessionDeps -> Web.Application
runDeleteUserHandler userId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HDeleteUser.run
    HDeleteUser.Handle
      { hDeleteUser = IDeleteUser.run deleteUserH
      , hPresent = presentDeletedUser
      , hAuthenticate = sdAuthenticate
      }
    userId
  where
    deleteUserH = IDeleteUser.Handle Database.deleteUser

runGetUsersHandler :: Deps -> SessionDeps -> Web.Application
runGetUsersHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetUsers.run
    HGetUsers.Handle
      { hGetUsers = IGetUsers.run getUsersH
      , hPresent = presentUsers dRepresentationBuilderHandle
      }
  where
    getUsersH =
      IGetUsers.Handle
        { hGetUsers = Database.getUsers
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runCreateTagHandler :: Deps -> SessionDeps -> Web.Application
runCreateTagHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HCreateTag.run
    HCreateTag.Handle
      { hCreateTag = ICreateTag.run createTagH
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      , hPresent = presentCreatedTag dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      }
  where
    createTagH =
      ICreateTag.Handle
        { hAuthorizationHandle = Core.Authorization.Impl.new
        , hCreateTagNamed = Database.createTagNamed
        , hFindTagNamed = Database.findTagNamed
        }

runGetTagHandler :: TagId -> Deps -> SessionDeps -> Web.Application
runGetTagHandler tagId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetTag.run
    HGetTag.Handle
      { hGetTag = IGetTag.run getTagH
      , hPresent = presentTag dRepresentationBuilderHandle
      }
    tagId
  where
    getTagH = IGetTag.Handle Database.getTag

runDeleteTagHandler :: TagId -> Deps -> SessionDeps -> Web.Application
runDeleteTagHandler tagId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HDeleteTag.run
    HDeleteTag.Handle
      { hDeleteTag = IDeleteTag.run deleteTagH
      , hAuthenticate = sdAuthenticate
      , hPresent = presentDeletedTag
      }
    tagId
  where
    deleteTagH = IDeleteTag.Handle Database.deleteTag

runPatchTagHandler :: TagId -> Deps -> SessionDeps -> Web.Application
runPatchTagHandler tagId' Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HPatchTag.run
    HPatchTag.Handle
      { hUpdateTag = IUpdateTag.run updateTagH
      , hAuthenticate = sdAuthenticate
      , hPresent = presentUpdatedTag dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      }
    tagId'
  where
    updateTagH =
      IUpdateTag.Handle
        { hFindTagNamed = fmap (fmap tagId) . Database.findTagNamed
        , hSetTagName = Database.setTagName
        }

runGetTagsHandler :: Deps -> SessionDeps -> Web.Application
runGetTagsHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetTags.run
    HGetTags.Handle
      { hGetTags = IGetTags.run getTagsH
      , hPresent = presentTags dRepresentationBuilderHandle
      }
  where
    getTagsH =
      IGetTags.Handle
        { hGetTags = Database.getTags
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runCreateDraftHandler :: Deps -> SessionDeps -> Web.Application
runCreateDraftHandler Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HCreateDraft.run
    HCreateDraft.Handle
      { hCreateDraft = ICreateDraft.run createDraftH
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      , hPresent =
          presentCreatedDraft dAppURIConfig dRepresentationBuilderHandle
      , hParseAppURI = Web.AppURI.parseAppURI dAppURIConfig
      , hAuthenticate = sdAuthenticate
      }
  where
    createDraftH =
      ICreateDraft.Handle
        { hAuthorizationHandle = Core.Authorization.Impl.new
        , hGetAuthorIdByUserIdIfExactlyOne =
            Database.getAuthorIdByUserIdIfExactlyOne
        , hCreateDraft = Database.createDraft
        , hRejectImageIfDisallowed =
            rejectDisallowedImage $ Cf.cfAllowedImageMimeTypes dConfig
        }

runCreateDraftFromNewsHandler ::
     NewsId -> Deps -> SessionDeps -> Web.Application
runCreateDraftFromNewsHandler newsId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HCreateDraftFromNews.run
    HCreateDraftFromNews.Handle
      { hCreateDraftFromNews = ICreateDraftFromNews.run createDraftFromNewsH
      , hPresent =
          presentCreatedDraft dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      }
    newsId
  where
    createDraftFromNewsH =
      ICreateDraftFromNews.Handle
        { hGetNewsAuthorId = Database.getNewsAuthorId
        , hCopyDraftFromNews = Database.copyDraftFromNews
        }

runGetDraftsOfNewsArticleHandler ::
     NewsId -> Deps -> SessionDeps -> Web.Application
runGetDraftsOfNewsArticleHandler newsId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetDraftsOfNewsArticle.run
    HGetDraftsOfNewsArticle.Handle
      { hGetDraftsOfNewsArticle = IGetDraftsOfNewsArticle.run interactorH
      , hPresent = presentDrafts dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      , hParsePageSpec = parsePageSpecM dPageSpecParserHandle
      }
    newsId
  where
    interactorH =
      IGetDraftsOfNewsArticle.Handle
        { hGetNewsAuthorId = Database.getNewsAuthorId
        , hGetDraftsCreatedFromNewsId = Database.getDraftsCreatedFromNewsId
        }

runPublishDraftHandler :: DraftId -> Deps -> SessionDeps -> Web.Application
runPublishDraftHandler draftId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HPublishDraft.run
    HPublishDraft.Handle
      { hPublishDraft = IPublishDraft.run interactorH
      , hPresent =
          presentCreatedOrUpdatedNewsItem
            dAppURIConfig
            dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
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

runGetDraftsHandler :: Maybe AuthorId -> Deps -> SessionDeps -> Web.Application
runGetDraftsHandler optAuthorId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetDrafts.run
    HGetDrafts.Handle
      { hGetDrafts = IGetDrafts.run interactorH
      , hAuthenticate = sdAuthenticate
      , hPresent = presentDrafts dRepresentationBuilderHandle
      }
    optAuthorId
  where
    interactorH =
      IGetDrafts.Handle
        { hGetDraftsOfAuthor = Database.getDraftsOfAuthor
        , hGetDraftsOfUser = Database.getDraftsOfUser
        , hAuthorizationHandle = Core.Authorization.Impl.new
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runGetDraftHandler :: DraftId -> Deps -> SessionDeps -> Web.Application
runGetDraftHandler draftId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetDraft.run
    HGetDraft.Handle
      { hGetDraft = IGetDraft.run interactorH
      , hAuthenticate = sdAuthenticate
      , hPresent = presentDraft dRepresentationBuilderHandle
      }
    draftId
  where
    interactorH = IGetDraft.Handle Database.getDraft

runDeleteDraftHandler :: DraftId -> Deps -> SessionDeps -> Web.Application
runDeleteDraftHandler draftId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HDeleteDraft.run
    HDeleteDraft.Handle
      { hDeleteDraft = IDeleteDraft.run interactorH
      , hAuthenticate = sdAuthenticate
      , hPresent = presentDeletedDraft
      }
    draftId
  where
    interactorH =
      IDeleteDraft.Handle
        { hGetDraftAuthor = Database.getDraftAuthor
        , hDeleteDraftAndItsContent = Database.deleteDraftAndItsContent
        }

runPatchDraftHandler :: DraftId -> Deps -> SessionDeps -> Web.Application
runPatchDraftHandler draftId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HPatchDraft.run
    HPatchDraft.Handle
      { hUpdateDraft = IUpdateDraft.run interactorH
      , hAuthenticate = sdAuthenticate
      , hPresent =
          presentUpdatedDraft dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      , hParseAppURI = Web.AppURI.parseAppURI dAppURIConfig
      }
    draftId
  where
    interactorH =
      IUpdateDraft.Handle
        { hGetDraftAuthor = Database.getDraftAuthor
        , hUpdateDraft = Database.updateDraft
        }

runCreateCommentHandler :: NewsId -> Deps -> SessionDeps -> Web.Application
runCreateCommentHandler newsId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HCreateComment.run
    HCreateComment.Handle
      { hCreateComment = ICreateComment.run interactorH
      , hPresent =
          presentCreatedComment dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = sdAuthenticate
      , hLoadJSONRequestBody = dLoadJSONRequestBody'
      }
    newsId
  where
    interactorH =
      ICreateComment.Handle
        { hCreateComment = Database.createComment
        , hGetCurrentTime = liftIO GCurrentTime.getIntegralSecondsTime
        }

runGetCommentHandler :: CommentId -> Deps -> SessionDeps -> Web.Application
runGetCommentHandler commentId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HGetComment.run
    HGetComment.Handle
      { hGetComment = IGetComment.run interactorH
      , hPresent = presentComment dRepresentationBuilderHandle
      }
    commentId
  where
    interactorH = IGetComment.Handle {hGetComment = Database.getComment}

runDeleteCommentHandler :: CommentId -> Deps -> SessionDeps -> Web.Application
runDeleteCommentHandler commentId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
  HDeleteComment.run
    HDeleteComment.Handle
      { hDeleteComment = IDeleteComment.run interactorH
      , hAuthenticate = sdAuthenticate
      , hPresent = presentDeletedComment
      }
    commentId
  where
    interactorH =
      IDeleteComment.Handle
        { hDeleteComment = Database.deleteComment
        , hGetCommentAuthor = Database.getCommentAuthor
        }

runGetCommentsForNewsHandler :: NewsId -> Deps -> SessionDeps -> Web.Application
runGetCommentsForNewsHandler newsId Deps {..} SessionDeps {..} =
  transactionApplicationToIOApplication sdDatabaseHandle $
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

-- | Creates an IO action and a logger handle. The IO action must be
-- forked in order for logging to work.
getLoggerHandle :: Cf.Config -> IO (Logger.Impl.Worker, Logger.Handle IO)
getLoggerHandle Cf.Config {..} = do
  hFileHandle <- getFileHandle cfLogFile
  Logger.Impl.new
    Logger.Impl.Handle {hFileHandle, hMinLevel = cfLoggerVerbosity}
  where
    getFileHandle (Cf.LogFilePath path) =
      openFile path AppendMode `catchS` \e ->
        die $ "While opening log file: " ++ displayException (e :: IOException)
    getFileHandle Cf.LogFileStdErr = pure stderr

transactionApplicationToIOApplication ::
     Database.Handle
  -> Web.GenericApplication Database.Transaction
  -> Web.GenericApplication IO
transactionApplicationToIOApplication h =
  Web.mapGenericApplication (Database.runTransactionRW h) liftIO

-- Commonly used dependencies dependent on Web.Session
data SessionDeps =
  SessionDeps
    { sdDatabaseHandle :: Database.Handle
    , sdAuthenticationHandle :: AuthenticationHandle IO
    , sdAuthenticate :: Maybe Credentials -> Database.Transaction AuthenticatedUser
    }

sessionDeps :: Deps -> Web.Session -> SessionDeps
sessionDeps Deps {..} session =
  SessionDeps
    { sdDatabaseHandle =
        sessionDatabaseHandle dDatabaseConnectionConfig dLoggerHandle session
    , sdAuthenticationHandle = dMakeAuthenticationHandle session
    -- ^ @todo: obsolete, should be deleted
    , sdAuthenticate = liftIO . authenticate (dMakeAuthenticationHandle session)
    }

sessionLoggerHandle :: Web.Session -> Logger.Handle IO -> Logger.Handle IO
sessionLoggerHandle Web.Session {..} =
  Logger.mapMessage $ \text -> "SID-" <> showAsText sessionId <> " " <> text

sessionDatabaseHandle ::
     DBConnManager.Config -> Logger.Handle IO -> Web.Session -> Database.Handle
sessionDatabaseHandle dbConnectionConfig loggerH session =
  Database.Handle
    { hConnectionConfig = dbConnectionConfig
    , hLoggerHandle = sessionLoggerHandle session loggerH
    }
