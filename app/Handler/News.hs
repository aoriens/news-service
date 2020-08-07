{-# LANGUAGE TemplateHaskell #-}

module Handler.News
  ( run
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

run :: Wai.Application
run _ respond =
  respond $
  Wai.responseBuilder
    Http.ok200
    [(Http.hContentType, "application/json")]
    (A.fromEncoding $ A.toEncoding stubNews)

data News =
  News
    { newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }

stubNews :: [News]
stubNews =
  [ News
      { newsTitle = "Title1"
      , newsDate = fromGregorian 2020 07 27
      , newsText = "A news text"
      }
  , News
      { newsTitle = "Title2"
      , newsDate = fromGregorian 2020 01 01
      , newsText = "A news text"
      }
  ]

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "news"}
    ''News)
