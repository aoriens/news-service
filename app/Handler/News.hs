{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.News
  ( run
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

run :: Wai.Application
run _ respond =
  respond $
  Wai.responseLBS
    Http.ok200
    [(Http.hContentType, "application/json")]
    (A.encode stubNews)

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
