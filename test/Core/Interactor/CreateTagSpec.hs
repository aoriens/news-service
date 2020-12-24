module Core.Interactor.CreateTagSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Exception
import Core.Interactor.CreateTag
import Core.Tag
import Data.IORef
import Test.AsyncExpectation
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it
      "should throw NoPermissionException and should not create a tag if the user is not an admin" $ do
      hasCreatedTag <- newIORef False
      let h =
            stubHandle
              { hCreateTagNamed =
                  \_ -> writeIORef hasCreatedTag True >> pure stubTag
              }
      run h someNonAdminUser "" `shouldThrow` isNoPermissionException
      readIORef hasCreatedTag `shouldReturn` False
    it
      "should pass the name to hCreateTagNamed if hFindTagNamed returned Nothing" $ do
      let expectedName = "a"
      shouldPassValue expectedName "hFindTagNamed" $ \onSuccess -> do
        let h =
              stubHandle
                { hFindTagNamed = \_ -> pure Nothing
                , hCreateTagNamed = \name -> onSuccess name >> pure stubTag
                }
        void $ run h someAdminUser expectedName
    it "should not invoke hCreateTagNamed if hFindTagNamed returned Just _" $ do
      createTagIsInvoked <- newIORef False
      let name = "a"
          h =
            stubHandle
              { hFindTagNamed = \_ -> pure $ Just stubTag
              , hCreateTagNamed =
                  \_ -> writeIORef createTagIsInvoked True >> pure stubTag
              }
      _ <- run h someAdminUser name
      readIORef createTagIsInvoked `shouldReturn` False
    it
      "should return ExistingTagFound with the tag returned from the gateway if such a tag found" $ do
      let name = "a"
          tag = stubTag {tagName = "pascal"}
          expectedResult = ExistingTagFound tag
          h = stubHandle {hFindTagNamed = \_ -> pure $ Just tag}
      r <- run h someAdminUser name
      r `shouldBe` expectedResult
    it
      "should return TagCreated with the tag returned from the gateway if no existing tag found" $ do
      let name = "a"
          tag = stubTag {tagName = "pascal"}
          expectedResult = TagCreated tag
          h =
            stubHandle
              { hFindTagNamed = \_ -> pure Nothing
              , hCreateTagNamed = \_ -> pure tag
              }
      r <- run h someAdminUser name
      r `shouldBe` expectedResult
    it "should throw CoreException if the tag name is empty" $ do
      let name = ""
          h = stubHandle
      run h someAdminUser name `shouldThrow` isIncorrectParameterException

stubTag :: Tag
stubTag = Tag {tagName = "q", tagId = TagId 1}

stubHandle :: Handle IO
stubHandle =
  Handle
    {hCreateTagNamed = \_ -> pure stubTag, hFindTagNamed = \_ -> pure Nothing}
