{-# LANGUAGE RankNTypes #-}

module Main
  ( main
  ) where

import qualified Config as Cf
import qualified Config.IO as CIO
import Control.Concurrent.Async
import Control.Exception
import Control.Exception.Sync
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
import qualified Core.Interactor.GetImage as IGetImage
import qualified Core.Interactor.GetNews as IGetNews
import qualified Core.Interactor.GetNewsList as IListNews
import qualified Core.Interactor.GetTag as IGetTag
import qualified Core.Interactor.GetTags as IGetTags
import qualified Core.Interactor.GetUser as IGetUser
import qualified Core.Interactor.GetUsers as IGetUsers
import qualified Core.Interactor.PublishDraft as IPublishDraft
import qualified Core.Interactor.UpdateAuthor as IUpdateAuthor
import qualified Core.Interactor.UpdateCategory as IUpdateCategory
import qualified Core.Interactor.UpdateTag as IUpdateTag
import Core.News
import Core.Pagination
import qualified Core.Pagination.Impl
import Core.Tag
import Core.User
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
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
import qualified Web.Handler.GetImage as HGetImage
import qualified Web.Handler.GetNews as HGetNews
import qualified Web.Handler.GetNewsList as HListNews
import qualified Web.Handler.GetTag as HGetTag
import qualified Web.Handler.GetTags as HGetTags
import qualified Web.Handler.GetUser as HGetUser
import qualified Web.Handler.GetUsers as HGetUsers
import qualified Web.Handler.PatchAuthor as HPatchAuthor
import qualified Web.Handler.PatchTag as HPatchTag
import qualified Web.Handler.PublishDraft as HPublishDraft
import qualified Web.Handler.UpdateCategory as HUpdateCategory
import qualified Web.JSONEncoder as JSONEncoder
import Web.Presenter
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
    , dLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Web.Request -> IO a
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
    NewsItemDraftsURI newsId -> [R.post $ runCreateDraftFromNewsHandler newsId]

runCreateAuthorHandler :: Deps -> SessionDeps -> Web.Application
runCreateAuthorHandler Deps {..} SessionDeps {..} =
  HCreateAuthor.run
    HCreateAuthor.Handle
      { hCreateAuthorHandle =
          ICreateAuthor.Handle
            { hAuthorizationHandle = Core.Authorization.Impl.new
            , hCreateAuthor = Database.createAuthor sdDatabaseHandle
            }
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent =
          presentCreatedAuthor dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      }

runGetAuthorsHandler :: Deps -> SessionDeps -> Web.Application
runGetAuthorsHandler Deps {..} SessionDeps {..} =
  HGetAuthors.run
    HGetAuthors.Handle
      { hGetAuthorsHandle =
          IGetAuthors.Handle
            { hAuthorizationHandle = Core.Authorization.Impl.new
            , hGetAuthors = Database.getAuthors sdDatabaseHandle
            , hPageSpecParserHandle = dPageSpecParserHandle
            }
      , hPresent = presentAuthors dRepresentationBuilderHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      }

runPatchAuthorHandler :: AuthorId -> Deps -> SessionDeps -> Web.Application
runPatchAuthorHandler authorId Deps {..} SessionDeps {..} =
  HPatchAuthor.run
    HPatchAuthor.Handle
      { hUpdateAuthorHandle =
          IUpdateAuthor.Handle
            { hAuthorizationHandle = Core.Authorization.Impl.new
            , hUpdateAuthor = Database.updateAuthor sdDatabaseHandle
            }
      , hPresent =
          presentUpdatedAuthor dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hAuthenticationHandle = sdAuthenticationHandle
      }
    authorId

runGetAuthorHandler :: AuthorId -> Deps -> SessionDeps -> Web.Application
runGetAuthorHandler authorId Deps {..} SessionDeps {..} =
  HGetAuthor.run
    HGetAuthor.Handle
      { hGetAuthorHandle =
          IGetAuthor.Handle {hGetAuthor = Database.getAuthor sdDatabaseHandle}
      , hPresent = presentAuthor dRepresentationBuilderHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      }
    authorId

runDeleteAuthorHandler :: AuthorId -> Deps -> SessionDeps -> Web.Application
runDeleteAuthorHandler authorId Deps {..} SessionDeps {..} =
  HDeleteAuthor.run
    HDeleteAuthor.Handle
      { hDeleteAuthorHandle =
          IDeleteAuthor.Handle
            { hDeleteAuthor = Database.deleteAuthor sdDatabaseHandle
            , hDeleteDraftsOfAuthor =
                Database.deleteDraftsOfAuthor sdDatabaseHandle
            }
      , hPresent = presentDeletedAuthor
      , hAuthenticationHandle = sdAuthenticationHandle
      }
    authorId

runCreateCategoryHandler :: Deps -> SessionDeps -> Web.Application
runCreateCategoryHandler Deps {..} SessionDeps {..} =
  HCreateCategory.run
    HCreateCategory.Handle
      { hCreateCategory =
          ICreateCategory.run
            ICreateCategory.Handle
              { hCreateCategory = Database.createCategory sdDatabaseHandle
              , hCategoryIdWithParentAndNameExists =
                  Database.categoryIdWithParentAndNameExists sdDatabaseHandle
              }
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent =
          presentCreatedCategory dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      }

runGetCategoryHandler :: CategoryId -> Deps -> SessionDeps -> Web.Application
runGetCategoryHandler categoryId Deps {..} SessionDeps {..} =
  HGetCategory.run
    HGetCategory.Handle
      { hGetCategoryHandle =
          IGetCategory.Handle
            {hGetCategory = Database.getCategory sdDatabaseHandle}
      , hPresent = presentCategory dRepresentationBuilderHandle
      }
    categoryId

runGetCategoriesHandler :: Deps -> SessionDeps -> Web.Application
runGetCategoriesHandler Deps {..} SessionDeps {..} =
  HGetCategories.run
    HGetCategories.Handle
      { hGetCategoriesHandle =
          IGetCategories.Handle
            { hGetCategories = Database.getCategories sdDatabaseHandle
            , hPageSpecParserHandle = dPageSpecParserHandle
            }
      , hPresent = presentCategories dRepresentationBuilderHandle
      }

runDeleteCategoryHandler :: CategoryId -> Deps -> SessionDeps -> Web.Application
runDeleteCategoryHandler categoryId Deps {..} SessionDeps {..} =
  HDeleteCategory.run
    HDeleteCategory.Handle
      { hDeleteCategoryHandle =
          IDeleteCategory.Handle
            { hGetCategory = Database.getCategory sdDatabaseHandle
            , hSetCategoryIdToNewsVersionsInCategoryAndDescendantCategories =
                Database.setCategoryIdToNewsVersionsInCategoryAndDescendantCategories
                  sdDatabaseHandle
            , hDeleteCategoryAndDescendants =
                Database.deleteCategoryAndDescendants sdDatabaseHandle
            }
      , hPresent = presentDeletedCategory
      , hAuthenticationHandle = sdAuthenticationHandle
      }
    categoryId

runUpdateCategoryHandler :: CategoryId -> Deps -> SessionDeps -> Web.Application
runUpdateCategoryHandler categoryId Deps {..} SessionDeps {..} =
  HUpdateCategory.run
    HUpdateCategory.Handle
      { hUpdateCategory =
          IUpdateCategory.run
            IUpdateCategory.Handle
              { hUpdateCategory = Database.updateCategory sdDatabaseHandle
              , hGetCategoryIdBySiblingAndName =
                  Database.getCategoryIdBySiblingAndName sdDatabaseHandle
              , hGetCategoryIdByParentAndName =
                  Database.getCategoryIdByParentAndName sdDatabaseHandle
              , hCategoryIsDescendantOf =
                  Database.categoryIsDescendantOf sdDatabaseHandle
              , hGetCategoryName = Database.getCategoryName sdDatabaseHandle
              }
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent =
          presentUpdatedCategory dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      }
    categoryId

runGetNewsListHandler :: Deps -> SessionDeps -> Web.Application
runGetNewsListHandler Deps {..} SessionDeps {..} =
  HListNews.run
    HListNews.Handle
      { hGetNewsHandle = interactorHandle
      , hJSONEncode = dJSONEncode
      , hPresent = presentNewsList dRepresentationBuilderHandle
      }
  where
    interactorHandle =
      IListNews.Handle
        { hGetNews = Database.getNewsList sdDatabaseHandle
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

runGetNewsHandler :: NewsId -> Deps -> SessionDeps -> Web.Application
runGetNewsHandler newsId Deps {..} SessionDeps {..} =
  HGetNews.run
    HGetNews.Handle
      { hGetNewsHandle =
          IGetNews.Handle {hGetNews = Database.getNews sdDatabaseHandle}
      , hPresent = presentNewsItem dRepresentationBuilderHandle
      }
    newsId

runCreateUserHandler :: Deps -> SessionDeps -> Web.Application
runCreateUserHandler Deps {..} SessionDeps {..} =
  HCreateUser.run
    HCreateUser.Handle
      { hCreateUserHandle = interactorHandle
      , hPresent = presentCreatedUser dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      }
  where
    interactorHandle =
      ICreateUser.Handle
        { hCreateUser = Database.createUser sdDatabaseHandle
        , hGenerateToken =
            GSecretToken.generateIO secretTokenConfig dSecretTokenIOState
        , hGetCurrentTime = GCurrentTime.getIntegralSecondsTime
        , hRejectImageIfDisallowed =
            rejectDisallowedImage $ Cf.cfAllowedImageMimeTypes dConfig
        }
    secretTokenConfig =
      GSecretToken.Config {cfTokenLength = Cf.cfSecretTokenLength dConfig}

runGetImageHandler :: ImageId -> Deps -> SessionDeps -> Web.Application
runGetImageHandler imageId Deps {..} SessionDeps {..} =
  HGetImage.run
    HGetImage.Handle
      { hGetImageHandle = IGetImage.Handle $ Database.getImage sdDatabaseHandle
      , hPresent = presentImage
      }
    imageId

runGetUserHandler :: UserId -> Deps -> SessionDeps -> Web.Application
runGetUserHandler userId Deps {..} SessionDeps {..} =
  HGetUser.run
    HGetUser.Handle
      { hGetUserHandle = IGetUser.Handle $ Database.getUser sdDatabaseHandle
      , hPresent = presentUser dRepresentationBuilderHandle
      }
    userId

runDeleteUserHandler :: UserId -> Deps -> SessionDeps -> Web.Application
runDeleteUserHandler userId Deps {..} SessionDeps {..} =
  HDeleteUser.run
    HDeleteUser.Handle
      { hDeleteUser =
          IDeleteUser.run . IDeleteUser.Handle $
          Database.deleteUser sdDatabaseHandle
      , hPresent = presentDeletedUser
      , hAuthenticationHandle = sdAuthenticationHandle
      }
    userId

runGetUsersHandler :: Deps -> SessionDeps -> Web.Application
runGetUsersHandler Deps {..} SessionDeps {..} =
  HGetUsers.run
    HGetUsers.Handle
      { hGetUsersHandle =
          IGetUsers.Handle
            { hGetUsers = Database.getUsers sdDatabaseHandle
            , hPageSpecParserHandle = dPageSpecParserHandle
            }
      , hPresent = presentUsers dRepresentationBuilderHandle
      }

runCreateTagHandler :: Deps -> SessionDeps -> Web.Application
runCreateTagHandler Deps {..} SessionDeps {..} =
  HCreateTag.run
    HCreateTag.Handle
      { hCreateTagHandle =
          ICreateTag.Handle
            { hAuthorizationHandle = Core.Authorization.Impl.new
            , hCreateTagNamed = Database.createTagNamed sdDatabaseHandle
            , hFindTagNamed = Database.findTagNamed sdDatabaseHandle
            }
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent = presentCreatedTag dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      }

runGetTagHandler :: TagId -> Deps -> SessionDeps -> Web.Application
runGetTagHandler tagId Deps {..} SessionDeps {..} =
  HGetTag.run
    HGetTag.Handle
      { hGetTagHandle = IGetTag.Handle $ Database.findTagById sdDatabaseHandle
      , hPresent = presentTag dRepresentationBuilderHandle
      }
    tagId

runDeleteTagHandler :: TagId -> Deps -> SessionDeps -> Web.Application
runDeleteTagHandler tagId Deps {..} SessionDeps {..} =
  HDeleteTag.run
    HDeleteTag.Handle
      { hDeleteTag =
          IDeleteTag.run $
          IDeleteTag.Handle $ Database.deleteTag sdDatabaseHandle
      , hAuthenticate = authenticate sdAuthenticationHandle
      , hPresent = presentDeletedTag
      }
    tagId

runPatchTagHandler :: TagId -> Deps -> SessionDeps -> Web.Application
runPatchTagHandler tagId' Deps {..} SessionDeps {..} =
  HPatchTag.run
    HPatchTag.Handle
      { hUpdateTag =
          IUpdateTag.run $
          IUpdateTag.Handle
            { hFindTagNamed =
                fmap (fmap tagId) . Database.findTagNamed sdDatabaseHandle
            , hSetTagName = Database.setTagName sdDatabaseHandle
            }
      , hAuthenticate = authenticate sdAuthenticationHandle
      , hPresent = presentUpdatedTag dAppURIConfig dRepresentationBuilderHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      }
    tagId'

runGetTagsHandler :: Deps -> SessionDeps -> Web.Application
runGetTagsHandler Deps {..} SessionDeps {..} =
  HGetTags.run
    HGetTags.Handle
      { hGetTagsHandle =
          IGetTags.Handle
            { hGetTags = Database.getTags sdDatabaseHandle
            , hPageSpecParserHandle = dPageSpecParserHandle
            }
      , hPresent = presentTags dRepresentationBuilderHandle
      }

runCreateDraftHandler :: Deps -> SessionDeps -> Web.Application
runCreateDraftHandler Deps {..} SessionDeps {..} =
  HCreateDraft.run
    HCreateDraft.Handle
      { hCreateDraftHandle =
          ICreateDraft.Handle
            { hAuthorizationHandle = Core.Authorization.Impl.new
            , hGetAuthorIdByUserIdIfExactlyOne =
                Database.getAuthorIdByUserIdIfExactlyOne sdDatabaseHandle
            , hCreateDraft = Database.createDraft sdDatabaseHandle
            , hRejectImageIfDisallowed =
                rejectDisallowedImage $ Cf.cfAllowedImageMimeTypes dConfig
            }
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      , hPresent =
          presentCreatedDraft dAppURIConfig dRepresentationBuilderHandle
      , hParseAppURI = Web.AppURI.parseAppURI dAppURIConfig
      , hAuthenticationHandle = sdAuthenticationHandle
      }

runCreateDraftFromNewsHandler ::
     NewsId -> Deps -> SessionDeps -> Web.Application
runCreateDraftFromNewsHandler newsId Deps {..} SessionDeps {..} =
  HCreateDraftFromNews.run
    HCreateDraftFromNews.Handle
      { hCreateDraftFromNews =
          ICreateDraftFromNews.run
            ICreateDraftFromNews.Handle
              { hGetNewsAuthor = Database.getNewsAuthorId sdDatabaseHandle
              , hCopyDraftFromNews = Database.copyDraftFromNews sdDatabaseHandle
              }
      , hPresent =
          presentCreatedDraft dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticate = authenticate sdAuthenticationHandle
      }
    newsId

runPublishDraftHandler :: DraftId -> Deps -> SessionDeps -> Web.Application
runPublishDraftHandler draftId Deps {..} SessionDeps {..} =
  HPublishDraft.run
    HPublishDraft.Handle
      { hPublishDraftHandle =
          IPublishDraft.Handle
            { hGetDraftAuthorAndNewsIdItWasCreatedFrom =
                Database.getDraftAuthorAndNewsIdItWasCreatedFrom
                  sdDatabaseHandle
            , hGetCurrentDay = getCurrentDay
            , hMakeDraftIntoNews = Database.makeDraftIntoNews sdDatabaseHandle
            , hOverwriteNewsWithDraft =
                Database.overwriteNewsWithDraft sdDatabaseHandle
            }
      , hPresent =
          presentCreatedNewsItem dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      }
    draftId

runGetDraftsHandler :: Maybe AuthorId -> Deps -> SessionDeps -> Web.Application
runGetDraftsHandler optAuthorId Deps {..} SessionDeps {..} =
  HGetDrafts.run
    HGetDrafts.Handle
      { hGetDraftsHandle =
          IGetDrafts.Handle
            { hGetDraftsOfAuthor = Database.getDraftsOfAuthor sdDatabaseHandle
            , hGetDraftsOfUser = Database.getDraftsOfUser sdDatabaseHandle
            , hAuthorizationHandle = Core.Authorization.Impl.new
            , hPageSpecParserHandle = dPageSpecParserHandle
            }
      , hAuthenticationHandle = sdAuthenticationHandle
      , hPresent = presentDrafts dRepresentationBuilderHandle
      }
    optAuthorId

runGetDraftHandler :: DraftId -> Deps -> SessionDeps -> Web.Application
runGetDraftHandler draftId Deps {..} SessionDeps {..} =
  HGetDraft.run
    HGetDraft.Handle
      { hGetDraftHandle = IGetDraft.Handle $ Database.getDraft sdDatabaseHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      , hPresent = presentDraft dRepresentationBuilderHandle
      }
    draftId

runDeleteDraftHandler :: DraftId -> Deps -> SessionDeps -> Web.Application
runDeleteDraftHandler draftId Deps {..} SessionDeps {..} =
  HDeleteDraft.run
    HDeleteDraft.Handle
      { hDeleteDraft =
          IDeleteDraft.run
            IDeleteDraft.Handle
              { hGetDraftAuthor = Database.getDraftAuthor sdDatabaseHandle
              , hDeleteDraftAndItsContent =
                  Database.deleteDraftAndItsContent sdDatabaseHandle
              }
      , hAuthenticate = authenticate sdAuthenticationHandle
      , hPresent = presentDeletedDraft
      }
    draftId

runCreateCommentHandler :: NewsId -> Deps -> SessionDeps -> Web.Application
runCreateCommentHandler newsId Deps {..} SessionDeps {..} =
  HCreateComment.run
    HCreateComment.Handle
      { hCreateCommentHandle =
          ICreateComment.Handle
            { hCreateComment = Database.createComment sdDatabaseHandle
            , hGetCurrentTime = GCurrentTime.getIntegralSecondsTime
            }
      , hPresent =
          presentCreatedComment dAppURIConfig dRepresentationBuilderHandle
      , hAuthenticationHandle = sdAuthenticationHandle
      , hLoadJSONRequestBody = dLoadJSONRequestBody
      }
    newsId

runGetCommentHandler :: CommentId -> Deps -> SessionDeps -> Web.Application
runGetCommentHandler commentId Deps {..} SessionDeps {..} =
  HGetComment.run
    HGetComment.Handle
      { hGetCommentHandle =
          IGetComment.Handle
            {hGetComment = Database.getComment sdDatabaseHandle}
      , hPresent = presentComment dRepresentationBuilderHandle
      }
    commentId

runDeleteCommentHandler :: CommentId -> Deps -> SessionDeps -> Web.Application
runDeleteCommentHandler commentId Deps {..} SessionDeps {..} =
  HDeleteComment.run
    HDeleteComment.Handle
      { hDeleteComment =
          IDeleteComment.run $
          IDeleteComment.Handle
            { hDeleteComment = Database.deleteComment sdDatabaseHandle
            , hGetCommentAuthor = Database.getCommentAuthor sdDatabaseHandle
            }
      , hAuthenticate = authenticate sdAuthenticationHandle
      , hPresent = presentDeletedComment
      }
    commentId

runGetCommentsForNewsHandler :: NewsId -> Deps -> SessionDeps -> Web.Application
runGetCommentsForNewsHandler newsId Deps {..} SessionDeps {..} =
  HGetCommentsForNews.run
    HGetCommentsForNews.Handle
      { hGetCommentsForNewsHandle =
          IGetCommentsForNews.Handle
            { hGetCommentsForNews = Database.getCommentsForNews sdDatabaseHandle
            , hPageSpecParserHandle = dPageSpecParserHandle
            }
      , hPresent = presentComments dRepresentationBuilderHandle
      }
    newsId

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

-- Commonly used dependencies dependent on Web.Session
data SessionDeps =
  SessionDeps
    { sdDatabaseHandle :: Database.Handle
    , sdAuthenticationHandle :: AuthenticationHandle IO
    }

sessionDeps :: Deps -> Web.Session -> SessionDeps
sessionDeps Deps {..} session =
  SessionDeps
    { sdDatabaseHandle =
        sessionDatabaseHandle dDatabaseConnectionConfig dLoggerHandle session
    , sdAuthenticationHandle = dMakeAuthenticationHandle session
    }

sessionLoggerHandle :: Web.Session -> Logger.Handle IO -> Logger.Handle IO
sessionLoggerHandle Web.Session {..} =
  Logger.mapMessage $ \text -> "SID-" <> T.pack (show sessionId) <> " " <> text

sessionDatabaseHandle ::
     DBConnManager.Config -> Logger.Handle IO -> Web.Session -> Database.Handle
sessionDatabaseHandle dbConnectionConfig loggerH session =
  Database.Handle
    { hConnectionConfig = dbConnectionConfig
    , hLoggerHandle = sessionLoggerHandle session loggerH
    }
