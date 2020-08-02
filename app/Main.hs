module Main
  ( main
  ) where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Builder as LBS
import qualified Handler.News
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

router :: R.Router
router = R.new $ R.ifPath ["news"] $ R.ifMethod Http.methodGet Handler.News.run

stubErrorResponse :: Http.Status -> [Http.Header] -> Wai.Response
stubErrorResponse status additionalHeaders =
  Wai.responseBuilder
    status
    ((Http.hContentType, "text/html") : additionalHeaders)
    body
  where
    body =
      "<!DOCTYPE html><html><body><h1>" <>
      LBS.stringUtf8 (show (Http.statusCode status)) <>
      " " <>
      LBS.byteString (Http.statusMessage status) <> "</h1></body></html>\n"
