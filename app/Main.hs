module Main
  ( main
  ) where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Builder as LBS
import qualified Database.ConnectionManager as DBConnManager
import qualified Gateway.News
import qualified Handler.News
import qualified Interactor.GetNews
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Router as R

main :: IO ()
main = do
  putStrLn "Server started"
  Warp.run 4000 routerApplication

routerApplication :: Wai.Application
routerApplication request =
  case R.route router request of
    R.HandlerResult handler -> handler request
    R.ResourceNotFoundResult -> ($ stubErrorResponse Http.notFound404 [])
    R.MethodNotSupportedResult knownMethods ->
      ($ stubErrorResponse
           Http.methodNotAllowed405
           [makeAllowHeader knownMethods])
  where
    makeAllowHeader methods = ("Allow", SBS.intercalate ", " methods)

stubErrorResponse :: Http.Status -> [Http.Header] -> Wai.Response
stubErrorResponse status additionalHeaders =
  Wai.responseBuilder
    status
    ((Http.hContentType, "text/html") : additionalHeaders)
    body
  where
    body =
      mconcat
        [ "<!DOCTYPE html><html><body><h1>"
        , LBS.stringUtf8 (show (Http.statusCode status))
        , " "
        , LBS.byteString (Http.statusMessage status) <> "</h1></body></html>\n"
        ]

router :: R.Router
router =
  R.new $ do
    R.ifPath ["news"] $ do
      R.ifMethod Http.methodGet $ Handler.News.run newsHandlerHandle

newsHandlerHandle :: Handler.News.Handle
newsHandlerHandle =
  Handler.News.Handle
    (Interactor.GetNews.Handle
       (Gateway.News.getNews
          Gateway.News.Handle
            { Gateway.News.hWithConnection =
                DBConnManager.withConnection dbConnectionConfig
            }))

dbConnectionConfig :: DBConnManager.Config
dbConnectionConfig =
  DBConnManager.makeConfig
    (DBConnManager.connectionSettingsWithDatabaseName "news")
