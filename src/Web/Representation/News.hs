{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.News
  ( newsRep
  ) where

import Core.Deletable
import Core.News
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Web.AppURI hiding (renderAppURI)
import Web.Representation.Author
import Web.Representation.Category
import Web.Representation.OneOf
import Web.Representation.Tag
import Web.RepresentationBuilder

newsRep :: News -> RepBuilder NewsRep
newsRep News {newsId, newsDate, newsContent = NewsVersion {..}} = do
  newsAuthor <-
    case nvAuthor of
      Deleted -> pure $ LeftRep "DELETED"
      Existing author -> RightRep <$> authorRep author
  newsCategory <- optCategoryRep nvCategory
  newsPhoto <- mapM (renderAppURI . ImageURI) nvMainPhotoId
  newsPhotos <- mapM (renderAppURI . ImageURI) $ toList nvAdditionalPhotoIds
  newsTags <- mapM tagRep $ toList nvTags
  pure
    NewsRep
      { newsNewsId = getNewsId newsId
      , newsTitle = nvTitle
      , newsDate
      , newsText = nvText
      , newsAuthor
      , newsCategory
      , newsPhoto
      , newsPhotos
      , newsTags
      }

data NewsRep =
  NewsRep
    { newsNewsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    , newsAuthor :: OneOfRep T.Text AuthorRep
    , newsCategory :: CategoryRep
    , newsPhoto :: Maybe AppURIRep
    , newsPhotos :: [AppURIRep]
    , newsTags :: [TagRep]
    }

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "news"}
    ''NewsRep)
