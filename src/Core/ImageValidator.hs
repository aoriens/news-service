module Core.ImageValidator
  ( rejectDisallowedImage
  ) where

import Control.Monad
import Control.Monad.Catch
import Core.Exception
import Core.Image
import Data.Foldable
import qualified Data.HashSet as Set
import Data.List
import qualified Data.Text as T

rejectDisallowedImage :: MonadThrow m => Set.HashSet T.Text -> Image -> m ()
rejectDisallowedImage allowedImageContentTypes Image {..} =
  when (imageContentType `notElem` allowedImageContentTypes) $
  throwM
    (DisallowedImageContentTypeException
       imageContentType
       (sort $ toList allowedImageContentTypes))
