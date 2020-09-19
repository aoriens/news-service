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
import qualified Core.Interactor.CreateAuthor as ICreateAuthor
import qualified Core.Interactor.CreateUser as ICreateUser
import qualified Core.Interactor.DeleteAuthor as IDeleteAuthor
import qualified Core.Interactor.DeleteUser as IDeleteUser
import qualified Core.Interactor.GetAuthor as IGetAuthor
import qualified Core.Interactor.GetAuthors as IGetAuthors
import qualified Core.Interactor.GetImage as IGetImage
import qualified Core.Interactor.GetNews as IGetNews
import qualified Core.Interactor.GetUser as IGetUser
import qualified Core.Interactor.GetUsers as IGetUsers
import qualified Core.Interactor.UpdateAuthor as IUpdateAuthor
import Core.Pagination
import qualified Core.Pagination.Impl
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import qualified Database
import qualified Database.ConnectionManager as DBConnManager
import qualified Gateway.Authors as GAuthors
import Gateway.CurrentTime as GCurrentTime
import qualified Gateway.Images as GImages
import qualified Gateway.News as GNews
import qualified Gateway.SecretToken as GSecretToken
import qualified Gateway.Users as GUsers
import qualified Logger
import qualified Logger.Impl
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Exit
import System.IO hiding (Handle)
import Web.AppURI
import qualified Web.Application
import qualified Web.Handler.CreateAuthor as HCreateAuthor
import qualified Web.Handler.CreateUser as HCreateUser
import qualified Web.Handler.DeleteAuthor as HDeleteAuthor
import qualified Web.Handler.DeleteUser as HDeleteUser
import qualified Web.Handler.GetAuthor as HGetAuthor
import qualified Web.Handler.GetAuthors as HGetAuthors
import qualified Web.Handler.GetImage as HGetImage
import qualified Web.Handler.GetNews as HGetNews
import qualified Web.Handler.GetUser as HGetUser
import qualified Web.Handler.GetUsers as HGetUsers
import qualified Web.Handler.PatchAuthor as HPatchAuthor
import qualified Web.JSONEncoder as JSONEncoder
import Web.Presenter
import Web.RepresentationBuilder
import qualified Web.RequestBodyLoader as RequestBodyLoader
import qualified Web.Router as R
import qualified Web.Types as Web

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
                                          Wai.Request -> IO a
    , dSecretTokenIOState :: GSecretToken.IOState
    , dRenderAppURI :: AppURI -> T.Text
    , dRepresentationBuilderHandle :: RepBuilderHandle
    , dMakeAuthHandle :: Web.Session -> AuthenticationHandle IO
    }

main :: IO ()
main = do
  (loggerWorker, deps@Deps {..}) <- getDeps
  webHandle <- getWebAppHandle deps
  race_ loggerWorker $ do
    Logger.info dLoggerHandle "Starting Warp"
    Warp.runSettings
      (Cf.cfWarpSettings dConfig)
      (Web.Application.application webHandle)

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
      dRenderAppURI = Web.AppURI.renderAppURI (Cf.cfAppURIConfig dConfig)
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
        , dRenderAppURI
        , dRepresentationBuilderHandle =
            RepBuilderHandle
              {hJSONEncode = dJSONEncode, hRenderAppURI = dRenderAppURI}
        , dMakeAuthHandle =
            \session ->
              AuthImpl.new
                AuthImpl.Handle
                  { hGetUserAuthData =
                      GUsers.getUserAuthData $
                      sessionDatabaseHandle'
                        dDatabaseConnectionConfig
                        dLoggerHandle
                        session
                  , hTokenMatchesHash = GSecretToken.tokenMatchesHash
                  , hLoggerHandle = sessionLoggerHandle session dLoggerHandle
                  }
        })

getWebAppHandle :: Deps -> IO Web.Application.Handle
getWebAppHandle deps@Deps {..} = do
  hState <- Web.Application.makeState
  pure
    Web.Application.Handle
      { hState
      , hLogger = (`sessionLoggerHandle` dLoggerHandle)
      , hRouter = router deps
      , hShowInternalExceptionInfoInResponses =
          Cf.cfShowInternalErrorInfoInResponse dConfig
      }

router :: Deps -> R.Router
router deps =
  R.new $ do
    R.pathPrefix ["authors"] $ do
      R.get $ HGetAuthor.run . getAuthorHandlerHandle deps
      R.delete $ HDeleteAuthor.run . deleteAuthorHandlerHandle deps
      R.patch $ HPatchAuthor.run . patchAuthorHandlerHandle deps
    R.path ["authors"] $ do
      R.get $ HGetAuthors.run . getAuthorsHandlerHandle deps
      R.post $ HCreateAuthor.run . createAuthorHandlerHandle deps
    R.path ["news"] $ R.get $ HGetNews.run . newsHandlerHandle deps
    R.pathPrefix ["users"] $ do
      R.get $ HGetUser.run . getUserHandlerHandle deps
      R.delete $ HDeleteUser.run . deleteUserHandlerHandle deps
    R.path ["users"] $ do
      R.get $ HGetUsers.run . getUsersHandlerHandle deps
      R.post $ HCreateUser.run . createUserHandle deps
    R.appURI $ \case
      (ImageURI imageId) ->
        R.get $ \session ->
          HGetImage.run (getImageHandlerHandle deps session) imageId

createAuthorHandlerHandle :: Deps -> Web.Session -> HCreateAuthor.Handle
createAuthorHandlerHandle deps@Deps {..} session =
  HCreateAuthor.Handle
    { hCreateAuthorHandle =
        ICreateAuthor.Handle
          { hAuthHandle = dMakeAuthHandle session
          , hCreateAuthor =
              GAuthors.createAuthor $ sessionDatabaseHandle deps session
          }
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    , hPresenter = authorCreatedPresenter dRepresentationBuilderHandle
    }

getAuthorsHandlerHandle :: Deps -> Web.Session -> HGetAuthors.Handle
getAuthorsHandlerHandle deps@Deps {..} session =
  HGetAuthors.Handle
    { hGetAuthorsHandle =
        IGetAuthors.Handle
          { hAuthHandle = dMakeAuthHandle session
          , hGetAuthors =
              GAuthors.getAuthors $ sessionDatabaseHandle deps session
          , hPageSpecParserHandle = dPageSpecParserHandle
          }
    , hPresenter = authorListPresenter dRepresentationBuilderHandle
    }

patchAuthorHandlerHandle :: Deps -> Web.Session -> HPatchAuthor.Handle
patchAuthorHandlerHandle deps@Deps {..} session =
  HPatchAuthor.Handle
    { hUpdateAuthorHandle =
        IUpdateAuthor.Handle
          { hAuthHandle = dMakeAuthHandle session
          , hUpdateAuthor =
              GAuthors.updateAuthor $ sessionDatabaseHandle deps session
          }
    , hPresenter = authorUpdatedPresenter dRepresentationBuilderHandle
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    }

getAuthorHandlerHandle :: Deps -> Web.Session -> HGetAuthor.Handle
getAuthorHandlerHandle deps@Deps {..} session =
  HGetAuthor.Handle
    { hGetAuthorHandle =
        IGetAuthor.Handle
          { hAuthHandle = dMakeAuthHandle session
          , hGetAuthor = GAuthors.getAuthor $ sessionDatabaseHandle deps session
          }
    , hPresenter = authorPresenter dRepresentationBuilderHandle
    }

deleteAuthorHandlerHandle :: Deps -> Web.Session -> HDeleteAuthor.Handle
deleteAuthorHandlerHandle deps@Deps {..} session =
  HDeleteAuthor.Handle
    { hDeleteAuthorHandle =
        IDeleteAuthor.Handle
          { hAuthHandle = dMakeAuthHandle session
          , hDeleteAuthor =
              GAuthors.deleteAuthor $ sessionDatabaseHandle deps session
          }
    , hPresenter = authorDeletedPresenter
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
        { hGetNews = GNews.getNews $ sessionDatabaseHandle deps session
        , hPageSpecParserHandle = dPageSpecParserHandle
        }

createUserHandle :: Deps -> Web.Session -> HCreateUser.Handle
createUserHandle deps@Deps {..} session =
  HCreateUser.Handle
    { hCreateUserHandle = interactorHandle
    , hPresenter = userCreatedPresenter dRepresentationBuilderHandle
    , hLoadJSONRequestBody = dLoadJSONRequestBody
    }
  where
    interactorHandle =
      ICreateUser.Handle
        { hCreateUser = GUsers.createUser $ sessionDatabaseHandle deps session
        , hGenerateToken =
            GSecretToken.generateIO secretTokenConfig dSecretTokenIOState
        , hGetCurrentTime = GCurrentTime.getIntegralSecondsTime
        , hAllowedImageContentTypes = Cf.cfAllowedImageMimeTypes dConfig
        }
    secretTokenConfig =
      GSecretToken.Config {cfTokenLength = Cf.cfSecretTokenLength dConfig}

getImageHandlerHandle :: Deps -> Web.Session -> HGetImage.Handle
getImageHandlerHandle deps@Deps {..} session =
  HGetImage.Handle
    { hGetImageHandle =
        IGetImage.Handle $ GImages.getImage $ sessionDatabaseHandle deps session
    , hPresenter = imagePresenter
    }

getUserHandlerHandle :: Deps -> Web.Session -> HGetUser.Handle
getUserHandlerHandle deps@Deps {..} session =
  HGetUser.Handle
    { hGetUserHandle =
        IGetUser.Handle $ GUsers.getUser $ sessionDatabaseHandle deps session
    , hPresenter = userPresenter dRepresentationBuilderHandle
    }

deleteUserHandlerHandle :: Deps -> Web.Session -> HDeleteUser.Handle
deleteUserHandlerHandle deps@Deps {..} session =
  HDeleteUser.Handle
    { hDeleteUserHandle =
        IDeleteUser.Handle
          { hDeleteUser = GUsers.deleteUser $ sessionDatabaseHandle deps session
          , hAuthHandle = dMakeAuthHandle session
          , hDefaultEntityListRange = dDefaultEntityListRange
          }
    , hPresenter = userDeletedPresenter
    }

getUsersHandlerHandle :: Deps -> Web.Session -> HGetUsers.Handle
getUsersHandlerHandle deps@Deps {..} session =
  HGetUsers.Handle
    { hGetUsersHandle =
        IGetUsers.Handle
          { hGetUsers = GUsers.getUsers $ sessionDatabaseHandle deps session
          , hPageSpecParserHandle = dPageSpecParserHandle
          }
    , hPresenter = userListPresenter dRepresentationBuilderHandle
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
