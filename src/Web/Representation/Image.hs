{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.Image
  ( ImageRep(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Representation.Base64

data ImageRep =
  ImageRep
    { imageRepBase64Data :: Base64
    , imageRepContentType :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      { A.fieldLabelModifier =
          A.camelTo2 '_' . fromJust . stripPrefix "imageRep"
      }
    ''ImageRep)
