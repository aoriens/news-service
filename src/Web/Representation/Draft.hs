{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.Draft
  ( draftRep
  , DraftRep(..)
  ) where

import Core.News
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.AppURI hiding (renderAppURI)
import Web.Representation.AppURI
import Web.Representation.Author
import Web.Representation.Category
import Web.Representation.Tag
import Web.RepresentationBuilder

draftRep :: NewsVersion -> RepBuilder DraftRep
draftRep NewsVersion {..} = do
  draftAuthor <- authorRep nvAuthor
  draftCategory <- categoryRep nvCategory
  draftPhoto <- mapM (renderAppURI . ImageURI) nvMainPhotoId
  draftPhotos <- mapM (renderAppURI . ImageURI) $ toList nvAdditionalPhotoIds
  draftTags <- mapM tagRep $ toList nvTags
  pure
    DraftRep
      { draftDraftId = getNewsVersionId nvId
      , draftTitle = nvTitle
      , draftText = nvText
      , draftAuthor
      , draftCategory
      , draftPhoto
      , draftPhotos
      , draftTags
      }

data DraftRep =
  DraftRep
    { draftDraftId :: Int32
    , draftTitle :: T.Text
    , draftText :: T.Text
    , draftAuthor :: AuthorRep
    , draftCategory :: CategoryRep
    , draftPhoto :: Maybe AppURIRep
    , draftPhotos :: [AppURIRep]
    , draftTags :: [TagRep]
    }

$(A.deriveToJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "draft"}
    ''DraftRep)
