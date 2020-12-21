{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.Image
  ( ImageRep(..)
  , imageFromRep
  , ExistingOrNewImageRep(..)
  , parseExistingOrNewImage
  ) where

import Control.Applicative
import Control.Monad.Catch
import Core.Image
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.AppURI hiding (parseAppURI)
import Web.Exception
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

parseExistingOrNewImage ::
     MonadThrow m
  => (T.Text -> Maybe AppURI)
  -> ExistingOrNewImageRep
  -> m (Either ImageId Image)
parseExistingOrNewImage parseAppURI image =
  case image of
    NewImage imageRep -> pure . Right $ imageFromRep imageRep
    ExistingImage (AppURIRep uriText) ->
      case parseAppURI uriText of
        Just (ImageURI imageId) -> pure $ Left imageId
        _ ->
          throwM . IncorrectParameterException $
          "URI does not match an image resource: " <> uriText

$(A.deriveFromJSON
    A.defaultOptions
      { A.fieldLabelModifier =
          A.camelTo2 '_' . fromJust . stripPrefix "imageRep"
      }
    ''ImageRep)
