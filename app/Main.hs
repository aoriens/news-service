{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Core.Authorization.Impl
import Core.ImageValidator
import qualified Core.Interactor.CreateAuthor as ICreateAuthor
import qualified Core.Interactor.CreateCategory as ICreateCategory
import qualified Core.Interactor.CreateDraft as ICreateDraft
import qualified Core.Interactor.CreateTag as ICreateTag
import qualified Core.Interactor.CreateUser as ICreateUser
import qualified Core.Interactor.DeleteAuthor as IDeleteAuthor
import qualified Core.Interactor.DeleteCategory as IDeleteCategory
import qualified Core.Interactor.DeleteUser as IDeleteUser
import qualified Core.Interactor.GetAuthor as IGetAuthor
import qualified Core.Interactor.GetAuthors as IGetAuthors
import qualified Core.Interactor.GetCategories as IGetCategories
import qualified Core.Interactor.GetCategory as IGetCategory
import qualified Core.Interactor.GetImage as IGetImage
import qualified Core.Interactor.GetNews as IGetNews
import qualified Core.Interactor.GetTag as IGetTag
import qualified Core.Interactor.GetTags as IGetTags
import qualified Core.Interactor.GetUser as IGetUser
import qualified Core.Interactor.GetUsers as IGetUsers
import qualified Core.Interactor.PublishDraft as IPublishDraft
import qualified Core.Interactor.UpdateAuthor as IUpdateAuthor
import Core.Pagination
import qualified Core.Pagination.Impl
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
import qualified Web.Handler.CreateDraft as HCreateDraft
import qualified Web.Handler.CreateTag as HCreateTag
import qualified Web.Handler.CreateUser as HCreateUser
import qualified Web.Handler.DeleteAuthor as HDeleteAuthor
import qualified Web.Handler.DeleteCategory as HDeleteCategory
import qualified Web.Handler.DeleteUser as HDeleteUser
import qualified Web.Handler.GetAuthor as HGetAuthor
import qualified Web.Handler.GetAuthors as HGetAuthors
import qualified Web.Handler.GetCategories as HGetCategories
import qualified Web.Handler.GetCategory as HGetCategory
import qualified Web.Handler.GetImage as HGetImage
import qualified Web.Handler.GetNews as HGetNews
import qualified Web.Handler.GetTag as HGetTag
import qualified Web.Handler.GetTags as HGetTags
import qualified Web.Handler.GetUser as HGetUser
import qualified Web.Handler.GetUsers as HGetUsers
import qualified Web.Handler.PatchAuthor as HPatchAuthor
import qualified Web.Handler.PublishDraft as HPublishDraft
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
    , dDefaultEntityListRange :: PageSpec
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
        , dDefaultEntityListRange =
            PageSpec (PageOffset 0) (Cf.cfMaxPageLimit dConfig)
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
                      sessionDatabaseHandle'
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

router :: Deps -> R.Router Web.EApplication
router deps =
  R.new $ \case
    ImageURI imageId ->
      R.get $ \session ->
        HGetImage.run (getImageHandlerHandle deps session) imageId
    UsersURI -> do
      R.get $ HGetUsers.run . getUsersHandlerHandle deps
      R.post $ HCreateUser.run . createUserHandle deps
    UserURI userId -> do
      R.get $ \session ->
        HGetUser.run (getUserHandlerHandle deps session) userId
      R.delete $ \session ->
        HDeleteUser.run (deleteUserHandlerHandle deps session) userId
    AuthorsURI -> do
      R.get $ HGetAuthors.run . getAuthorsHandlerHandle deps
      R.post $ HCreateAuthor.run . createAuthorHandlerHandle deps
    AuthorURI authorId -> do
      R.get $ \session ->
        HGetAuthor.run (getAuthorHandlerHandle deps session) authorId
      R.delete $ \session ->
        HDeleteAuthor.run (deleteAuthorHandlerHandle deps session) authorId
      R.patch $ \session ->
        HPatchAuthor.run (patchAuthorHandlerHandle deps session) authorId
    CategoriesURI -> do
      R.get $ HGetCategories.run . getCategoriesHandlerHandle deps
      R.post $ HCreateCategory.run . createCategoryHandlerHandle deps
    CategoryURI categoryId -> do
      R.get $ \session ->
        HGetCategory.run (getCategoryHandlerHandle deps session) categoryId
      R.delete $ \session ->
        HDeleteCategory.run
          (deleteCategoryHandlerHandle deps session)
          categoryId
    NewsURI -> R.get $ HGetNews.run . newsHandlerHandle deps
    NewsItemURI _ -> pure ()
    TagsURI -> do
      R.get $ HGetTags.run . getTagsHandlerHandle deps
      R.post $ HCreateTag.run . createTagHandlerHandle deps
    TagURI tagId' ->
      R.get $ \session -> HGetTag.run (getTagHandlerHandle deps session) tagId'
    DraftsURI -> R.post $ HCreateDraft.run . createDraftHandlerHandle deps
    DraftURI _ -> pure ()
    PublishDraftURI draftId ->
      R.post $ \session ->
        HPublishDraft.run (publishDraftHandlerHandle deps session) draftId

createAuthorHandlerHandle :: Deps -> Web.Session -> HCreateAuthor.Handle
createAuthorHandlerHandle deps@Deps {..} session =
  HCreateAuthor.Handle
    { hCreateAuthorHandle =
        ICreateAuthor.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hCreateAuthor =
              Database.createAuthor $ sessionDatabaseHandle deps session
          }
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    , hPresenter =
        authorCreatedPresenter dAppURIConfig dRepresentationBuilderHandle
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

getAuthorsHandlerHandle :: Deps -> Web.Session -> HGetAuthors.Handle
getAuthorsHandlerHandle deps@Deps {..} session =
  HGetAuthors.Handle
    { hGetAuthorsHandle =
        IGetAuthors.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hGetAuthors =
              Database.getAuthors $ sessionDatabaseHandle deps session
          , hPageSpecParserHandle = dPageSpecParserHandle
          }
    , hPresenter = authorListPresenter dRepresentationBuilderHandle
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

patchAuthorHandlerHandle :: Deps -> Web.Session -> HPatchAuthor.Handle
patchAuthorHandlerHandle deps@Deps {..} session =
  HPatchAuthor.Handle
    { hUpdateAuthorHandle =
        IUpdateAuthor.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hUpdateAuthor =
              Database.updateAuthor $ sessionDatabaseHandle deps session
          }
    , hPresenter =
        authorUpdatedPresenter dAppURIConfig dRepresentationBuilderHandle
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

getAuthorHandlerHandle :: Deps -> Web.Session -> HGetAuthor.Handle
getAuthorHandlerHandle deps@Deps {..} session =
  HGetAuthor.Handle
    { hGetAuthorHandle =
        IGetAuthor.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hGetAuthor = Database.getAuthor $ sessionDatabaseHandle deps session
          }
    , hPresenter = authorPresenter dRepresentationBuilderHandle
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

deleteAuthorHandlerHandle :: Deps -> Web.Session -> HDeleteAuthor.Handle
deleteAuthorHandlerHandle deps@Deps {..} session =
  HDeleteAuthor.Handle
    { hDeleteAuthorHandle =
        IDeleteAuthor.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hDeleteAuthor =
              Database.deleteAuthor $ sessionDatabaseHandle deps session
          }
    , hPresenter = authorDeletedPresenter
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

createCategoryHandlerHandle :: Deps -> Web.Session -> HCreateCategory.Handle
createCategoryHandlerHandle deps@Deps {..} session =
  HCreateCategory.Handle
    { hCreateCategoryHandle =
        ICreateCategory.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hCreateCategory =
              Database.createCategory $ sessionDatabaseHandle deps session
          }
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    , hPresenter =
        categoryCreatedPresenter dAppURIConfig dRepresentationBuilderHandle
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

getCategoryHandlerHandle :: Deps -> Web.Session -> HGetCategory.Handle
getCategoryHandlerHandle deps@Deps {..} session =
  HGetCategory.Handle
    { hGetCategoryHandle =
        IGetCategory.Handle
          { hGetCategory =
              Database.getCategory $ sessionDatabaseHandle deps session
          }
    , hPresenter = categoryPresenter dRepresentationBuilderHandle
    }

getCategoriesHandlerHandle :: Deps -> Web.Session -> HGetCategories.Handle
getCategoriesHandlerHandle deps@Deps {..} session =
  HGetCategories.Handle
    { hGetCategoriesHandle =
        IGetCategories.Handle
          { hGetCategories =
              Database.getCategories $ sessionDatabaseHandle deps session
          , hPageSpecParserHandle = dPageSpecParserHandle
          }
    , hPresenter = categoryListPresenter dRepresentationBuilderHandle
    }

deleteCategoryHandlerHandle :: Deps -> Web.Session -> HDeleteCategory.Handle
deleteCategoryHandlerHandle deps@Deps {..} session =
  HDeleteCategory.Handle
    { hDeleteCategoryHandle =
        IDeleteCategory.Handle
          { hDeleteCategory =
              Database.deleteCategory $ sessionDatabaseHandle deps session
          , hAuthorizationHandle = Core.Authorization.Impl.new
          , hDefaultEntityListRange = dDefaultEntityListRange
          }
    , hPresenter = categoryDeletedPresenter
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

newsHandlerHandle :: Deps -> Web.Session -> HGetNews.Handle
newsHandlerHandle deps@Deps {..} session =
  HGetNews.Handle
    { hGetNewsHandle = interactorHandle
    , hJSONEncode = dJSONEncode
    , hPresenter = newsListPresenter dRepresentationBuilderHandle
    }
  where
    interactorHandle =
      IGetNews.Handle
        { hGetNews = Database.getNewsList $ sessionDatabaseHandle deps session
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

createUserHandle :: Deps -> Web.Session -> HCreateUser.Handle
createUserHandle deps@Deps {..} session =
  HCreateUser.Handle
    { hCreateUserHandle = interactorHandle
    , hPresenter =
        userCreatedPresenter dAppURIConfig dRepresentationBuilderHandle
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    }
  where
    interactorHandle =
      ICreateUser.Handle
        { hCreateUser = Database.createUser $ sessionDatabaseHandle deps session
        , hGenerateToken =
            GSecretToken.generateIO secretTokenConfig dSecretTokenIOState
        , hGetCurrentTime = GCurrentTime.getIntegralSecondsTime
        , hRejectDisallowedImage =
            rejectDisallowedImage $ Cf.cfAllowedImageMimeTypes dConfig
        }
    secretTokenConfig =
      GSecretToken.Config {cfTokenLength = Cf.cfSecretTokenLength dConfig}

getImageHandlerHandle :: Deps -> Web.Session -> HGetImage.Handle
getImageHandlerHandle deps@Deps {..} session =
  HGetImage.Handle
    { hGetImageHandle =
        IGetImage.Handle $
        Database.getImage $ sessionDatabaseHandle deps session
    , hPresenter = imagePresenter
    }

getUserHandlerHandle :: Deps -> Web.Session -> HGetUser.Handle
getUserHandlerHandle deps@Deps {..} session =
  HGetUser.Handle
    { hGetUserHandle =
        IGetUser.Handle $ Database.getUser $ sessionDatabaseHandle deps session
    , hPresenter = userPresenter dRepresentationBuilderHandle
    }

deleteUserHandlerHandle :: Deps -> Web.Session -> HDeleteUser.Handle
deleteUserHandlerHandle deps@Deps {..} session =
  HDeleteUser.Handle
    { hDeleteUserHandle =
        IDeleteUser.Handle
          { hDeleteUser =
              Database.deleteUser $ sessionDatabaseHandle deps session
          , hAuthorizationHandle = Core.Authorization.Impl.new
          , hDefaultEntityListRange = dDefaultEntityListRange
          }
    , hPresenter = userDeletedPresenter
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

getUsersHandlerHandle :: Deps -> Web.Session -> HGetUsers.Handle
getUsersHandlerHandle deps@Deps {..} session =
  HGetUsers.Handle
    { hGetUsersHandle =
        IGetUsers.Handle
          { hGetUsers = Database.getUsers $ sessionDatabaseHandle deps session
          , hPageSpecParserHandle = dPageSpecParserHandle
          }
    , hPresenter = userListPresenter dRepresentationBuilderHandle
    }

createTagHandlerHandle :: Deps -> Web.Session -> HCreateTag.Handle
createTagHandlerHandle deps@Deps {..} session =
  HCreateTag.Handle
    { hCreateTagHandle =
        ICreateTag.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hCreateTagNamed =
              Database.createTagNamed $ sessionDatabaseHandle deps session
          , hFindTagByName =
              Database.findTagByName $ sessionDatabaseHandle deps session
          }
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    , hPresenter =
        tagCreatedPresenter dAppURIConfig dRepresentationBuilderHandle
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

getTagHandlerHandle :: Deps -> Web.Session -> HGetTag.Handle
getTagHandlerHandle deps@Deps {..} session =
  HGetTag.Handle
    { hGetTagHandle =
        IGetTag.Handle $
        Database.findTagById $ sessionDatabaseHandle deps session
    , hPresenter = tagPresenter dRepresentationBuilderHandle
    }

getTagsHandlerHandle :: Deps -> Web.Session -> HGetTags.Handle
getTagsHandlerHandle deps@Deps {..} session =
  HGetTags.Handle
    { hGetTagsHandle =
        IGetTags.Handle
          { hGetTags = Database.getTags $ sessionDatabaseHandle deps session
          , hPageSpecParserHandle = dPageSpecParserHandle
          }
    , hPresenter = tagListPresenter dRepresentationBuilderHandle
    }

createDraftHandlerHandle :: Deps -> Web.Session -> HCreateDraft.Handle
createDraftHandlerHandle deps@Deps {..} session =
  HCreateDraft.Handle
    { hCreateDraftHandle =
        ICreateDraft.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hGetAuthorIdByUserIdIfExactlyOne =
              Database.getAuthorIdByUserIdIfExactlyOne $
              sessionDatabaseHandle deps session
          , hCreateNewsVersion =
              Database.createNewsVersion $ sessionDatabaseHandle deps session
          , hRejectDisallowedImage =
              rejectDisallowedImage $ Cf.cfAllowedImageMimeTypes dConfig
          }
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    , hPresenter =
        draftCreatedPresenter dAppURIConfig dRepresentationBuilderHandle
    , hParseAppURI = Web.AppURI.parseAppURI dAppURIConfig
    , hAuthenticationHandle = dMakeAuthenticationHandle session
    }

publishDraftHandlerHandle :: Deps -> Web.Session -> HPublishDraft.Handle
publishDraftHandlerHandle deps@Deps {..} session =
  HPublishDraft.Handle
    { hPublishDraftHandle =
        IPublishDraft.Handle
          { hAuthorizationHandle = Core.Authorization.Impl.new
          , hGetDraftAuthor =
              Database.getDraftAuthor $ sessionDatabaseHandle deps session
          , hGetCurrentDay = getCurrentDay
          , hCreateNews =
              Database.createNews $ sessionDatabaseHandle deps session
          }
    , hPresenter =
        newsCreatedPresenter dAppURIConfig dRepresentationBuilderHandle
    , hAuthenticationHandle = dMakeAuthenticationHandle session
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

sessionLoggerHandle :: Web.Session -> Logger.Handle IO -> Logger.Handle IO
sessionLoggerHandle Web.Session {..} =
  Logger.mapMessage $ \text -> "SID-" <> T.pack (show sessionId) <> " " <> text

sessionDatabaseHandle :: Deps -> Web.Session -> Database.Handle
sessionDatabaseHandle Deps {..} =
  sessionDatabaseHandle' dDatabaseConnectionConfig dLoggerHandle

sessionDatabaseHandle' ::
     DBConnManager.Config -> Logger.Handle IO -> Web.Session -> Database.Handle
sessionDatabaseHandle' dbConnectionConfig loggerH session =
  Database.Handle
    { hConnectionConfig = dbConnectionConfig
    , hLoggerHandle = sessionLoggerHandle session loggerH
    }
