{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.Image
  ( ImageRep(..)
  , imageFromRep
  , ExistingOrNewImageRep(..)
  ) where

import Control.Applicative
import Core.Image
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Representation.AppURI
import Web.Representation.Base64

data ImageRep =
  ImageRep
    { imageRepBase64Data :: Base64
    , imageRepContentType :: T.Text
    }

imageFromRep :: ImageRep -> Image
imageFromRep ImageRep {..} =
  Image
    { imageData = unBase64 imageRepBase64Data
    , imageContentType = imageRepContentType
    }

data ExistingOrNewImageRep
  = ExistingImage AppURIRep
  | NewImage ImageRep

instance A.FromJSON ExistingOrNewImageRep where
  parseJSON o = ExistingImage <$> A.parseJSON o <|> NewImage <$> A.parseJSON o

$(A.deriveFromJSON
    A.defaultOptions
      { A.fieldLabelModifier =
          A.camelTo2 '_' . fromJust . stripPrefix "imageRep"
      }
    ''ImageRep)
