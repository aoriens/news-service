{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.News
  ( newsRepresentation
  ) where

import qualified Core.Interactor.GetNews as Core
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Web.RepresentationBuilder

newsRepresentation :: Core.News -> RepBuilder News
newsRepresentation Core.News {..} = pure News {newsNewsId = newsId, ..}

data News =
  News
    { newsNewsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "news"}
    ''News)
