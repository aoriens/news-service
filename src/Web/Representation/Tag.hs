{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.Tag
  ( Tag
  , tagRep
  ) where

import Core.Tag
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.RepresentationBuilder

data TagRep =
  TagRep
    { tagId :: Int32
    , tagName :: T.Text
    }

tagRep :: Tag -> RepBuilder TagRep
tagRep Tag {..} = pure TagRep {tagId = getTagId tagId, tagName}

$(A.deriveToJSON
    A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "tag"
      , A.omitNothingFields = True
      }
    ''TagRep)
