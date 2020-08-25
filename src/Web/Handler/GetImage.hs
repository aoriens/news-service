{-# LANGUAGE RecordWildCards #-}

module Web.Handler.GetImage
  ( run
  , Handle(..)
  ) where

import Control.Exception
import Core.Image
import qualified Core.Interactor.GetImage as I
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Util as Wai
import qualified Web.Exception as E

newtype Handle =
  Handle
    { hGetImage_Handle :: I.Handle IO
    }

run :: Handle -> ImageId -> Wai.Application
run Handle {..} imageId _ respond = do
  optImage <- I.run hGetImage_Handle imageId
  Image {..} <- maybe (throwIO E.NotFoundException) pure optImage
  respond $
    Wai.simpleResponseStream
      Http.ok200
      [(Http.hContentType, T.encodeUtf8 imageContentType)]
      (pure $ BB.byteString imageData)