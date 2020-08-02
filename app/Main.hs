{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
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
router = R.new $ R.ifPath ["news"] $ R.ifMethod Http.methodGet handlerGetNews

handlerGetNews :: Wai.Application
handlerGetNews _ respond =
  respond $
  Wai.responseLBS
    Http.ok200
    [(Http.hContentType, "application/json")]
    (A.encode stubNews)

stubErrorResponse :: Http.Status -> [Http.Header] -> Wai.Response
stubErrorResponse status additionalHeaders =
  Wai.responseLBS
    status
    ((Http.hContentType, "text/html") : additionalHeaders)
    body
  where
    body =
      "<!DOCTYPE html><html><body><h1>" <>
      LBS.pack (show (Http.statusCode status)) <>
      " " <>
      LBS.fromStrict (Http.statusMessage status) <> "</h1></body></html>\n"

data News =
  News
    { newsTitle :: Text
    , newsDate :: UTCTime
    , newsText :: Text
    , newsImage :: String
    }

stubNews :: [News]
stubNews =
  [ News
      { newsTitle = "Title1"
      , newsDate = utcMidnight 2020 07 27
      , newsText = "A news text"
      , newsImage = "http://example.com/image1.png"
      }
  , News
      { newsTitle = "Title2"
      , newsDate = utcMidnight 2020 01 01
      , newsText = "A news text"
      , newsImage = "http://example.com/image1.png"
      }
  ]
  where
    utcMidnight year month day = UTCTime (fromGregorian year month day) 0

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "news"}
    ''News)
