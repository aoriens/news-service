{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  putStrLn "Server started"
  Warp.run 4000 application

application :: Wai.Application
application request respond =
  case HM.lookup (Wai.pathInfo request) handlersTable of
    Nothing -> respond $ stubErrorResponse Http.notFound404 []
    Just subtable ->
      case HM.lookup (Wai.requestMethod request) subtable of
        Just handler -> handler request respond
        Nothing ->
          respond $
          stubErrorResponse
            Http.methodNotAllowed405
            [makeAllowHeader (HM.keys subtable)]
  where
    makeAllowHeader methods = ("Allow", SBS.intercalate ", " (sort methods))

type UrlPath = [Text]

handlersTable :: HashMap UrlPath (HashMap Http.Method Wai.Application)
handlersTable =
  HM.fromList [(["news"], HM.fromList [(Http.methodGet, handlerGetNews)])]

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
  deriving (Generic)

instance A.ToJSON News where
  toJSON = A.genericToJSON (toJSONOptionsWithFieldPrefix "news")
  toEncoding = A.genericToEncoding (toJSONOptionsWithFieldPrefix "news")

toJSONOptionsWithFieldPrefix :: String -> A.Options
toJSONOptionsWithFieldPrefix prefix =
  A.defaultOptions {A.fieldLabelModifier = formatField}
  where
    formatField fieldName =
      A.camelTo2 '_' .
      fromMaybe
        (error
           ("The field '" ++
            fieldName ++ "' must be prefixed with '" ++ prefix ++ "'")) .
      stripPrefix prefix $
      fieldName

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
