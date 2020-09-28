{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.News
  ( newsRep
  ) where

import Core.News
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Web.RepresentationBuilder

newsRep :: News -> RepBuilder NewsRep
newsRep News {..} = pure NewsRep {newsNewsId = newsId, ..}

data NewsRep =
  NewsRep
    { newsNewsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "news"}
    ''NewsRep)
