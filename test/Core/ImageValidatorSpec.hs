module Core.ImageValidatorSpec
  ( spec
  ) where

import Control.Exception
import Core.Exception
import Core.Image
import Core.ImageValidator
import qualified Data.HashSet as Set
import Test.Hspec

spec :: Spec
spec =
  describe "rejectDisallowedImage" $ do
    it
      "should throw DisallowedImageContentTypeException if image content type is not in the allowed list" $ do
      let allowedTypes = Set.fromList ["good"]
          disallowedType = "bad"
          image = Image {imageContentType = disallowedType, imageData = ""}
      r <- try $ rejectDisallowedImage allowedTypes image
      r `shouldSatisfy`
        either isDisallowedImageContentTypeException (const False)
    it "should not throw if image content type is in the allowed list" $ do
      let allowedType = "good"
          allowedTypes = Set.fromList [allowedType]
          image = Image {imageContentType = allowedType, imageData = ""}
      rejectDisallowedImage allowedTypes image `shouldReturn` ()
