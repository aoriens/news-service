{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import qualified Config as Cf
import Data.String
import qualified Database.ConnectionManager as DBConnManager
import qualified Gateway.News
import qualified Interactor.GetNews
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Handler.Warp as Warp
import qualified Router as R
import qualified Web
import qualified Web.Handler.News as HNews

-- The local environment containing configuration loaded from IO and
-- maybe some dependencies. It's purpose to be passed to pure
-- functions **in this module**, keeping extensibility in the number
-- of fields and avoiding to add excess parameters to 100500 function
-- signatures.
newtype Env =
  Env DBConnManager.Config

main :: IO ()
main = do
  config <- Cf.getConfig
  let settings = warpSettings config
      env = makeEnv config
  putStrLn "Server started"
  Warp.runSettings settings (Web.application (router env))

warpSettings :: Cf.Config -> Warp.Settings
warpSettings Cf.Config {..} =
  maybe id (Warp.setServerName . fromString) cfServerName .
  maybe id (Warp.setHost . fromString) cfServerHostPreference .
  maybe id Warp.setPort cfServerPort $
  Warp.setHost "localhost" Warp.defaultSettings

makeEnv :: Cf.Config -> Env
makeEnv = Env . makeDBConnectionConfig

makeDBConnectionConfig :: Cf.Config -> DBConnManager.Config
makeDBConnectionConfig Cf.Config {..} =
  DBConnManager.makeConfig $
  (DBConnManager.connectionSettingsWithDatabaseName $ fromString cfDatabaseName)
    { DBConnManager.settingsHost = fromString <$> cfDatabaseHost
    , DBConnManager.settingsPort = cfDatabasePort
    , DBConnManager.settingsUser = fromString <$> cfDatabaseUser
    , DBConnManager.settingsPassword = fromString <$> cfDatabasePassword
    }

router :: Env -> R.Router
router env =
  R.new $ do
    R.ifPath ["news"] $ do
      R.ifMethod Http.methodGet $ HNews.run (newsHandlerHandle env)

newsHandlerHandle :: Env -> HNews.Handle
newsHandlerHandle (Env dbConnectionConfig) =
  HNews.Handle
    (Interactor.GetNews.Handle
       (Gateway.News.getNews
          Gateway.News.Handle
            { Gateway.News.hWithConnection =
                DBConnManager.withConnection dbConnectionConfig
            }))
