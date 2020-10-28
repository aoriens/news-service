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
import Web.AppURI hiding (renderAppURI)
import Web.Representation.Author
import Web.Representation.Category
import Web.RepresentationBuilder

newsRep :: News -> RepBuilder NewsRep
newsRep News {newsId, newsDate, newsVersion = NewsVersion {..}} = do
  newsAuthor <- authorRep nvAuthor
  newsCategory <- categoryRep nvCategory
  newsPhoto <- renderMaybeAppURI (ImageURI <$> nvMainPhotoId)
  pure
    NewsRep
      { newsNewsId = getNewsId newsId
      , newsTitle = nvTitle
      , newsDate
      , newsText = nvText
      , newsAuthor
      , newsCategory
      , newsPhoto
      }

data NewsRep =
  NewsRep
    { newsNewsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    , newsAuthor :: AuthorRep
    , newsCategory :: CategoryRep
    , newsPhoto :: Maybe AppURIRep
    }

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "news"}
    ''NewsRep)
