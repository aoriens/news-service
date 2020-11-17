module Core.Interactor.CreateTagSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Authorization
import Core.Authorization.Test
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
    itShouldAuthorizeBeforeOperation AdminPermission $ \authUser authorizationHandle onSuccess -> do
      let name = "a"
          h =
            stubHandle
              { hCreateTagNamed = \_ -> onSuccess >> pure stubTag
              , hFindTagByName = \_ -> onSuccess >> pure Nothing
              , hAuthorizationHandle = authorizationHandle
              }
      void $ run h authUser name
    it "should pass the name to hFindTagByName in a normal case" $ do
      let expectedName = "a"
      shouldPassValue expectedName "hFindTagByName" $ \onSuccess -> do
        let h =
              stubHandle
                {hFindTagByName = \name -> onSuccess name >> pure Nothing}
        void $ run h anyAuthUser expectedName
    it
      "should pass the name to hCreateTagNamed if hFindTagByName returned Nothing" $ do
      let expectedName = "a"
      shouldPassValue expectedName "hFindTagByName" $ \onSuccess -> do
        let h =
              stubHandle
                { hFindTagByName = \_ -> pure Nothing
                , hCreateTagNamed = \name -> onSuccess name >> pure stubTag
                }
        void $ run h anyAuthUser expectedName
    it "should not invoke hCreateTagNamed if hFindTagByName returned Just _" $ do
      createTagIsInvoked <- newIORef False
      let name = "a"
          h =
            stubHandle
              { hFindTagByName = \_ -> pure $ Just stubTag
              , hCreateTagNamed =
                  \_ -> writeIORef createTagIsInvoked True >> pure stubTag
              }
      _ <- run h anyAuthUser name
      readIORef createTagIsInvoked `shouldReturn` False
    it
      "should return ExistingTagFound with the tag returned from the gateway if such a tag found" $ do
      let name = "a"
          tag = stubTag {tagName = "pascal"}
          expectedResult = ExistingTagFound tag
          h = stubHandle {hFindTagByName = \_ -> pure $ Just tag}
      r <- run h anyAuthUser name
      r `shouldBe` expectedResult
    it
      "should return TagCreated with the tag returned from the gateway if no existing tag found" $ do
      let name = "a"
          tag = stubTag {tagName = "pascal"}
          expectedResult = TagCreated tag
          h =
            stubHandle
              { hFindTagByName = \_ -> pure Nothing
              , hCreateTagNamed = \_ -> pure tag
              }
      r <- run h anyAuthUser name
      r `shouldBe` expectedResult
    it "should throw CoreException if the tag name is empty" $ do
      let name = ""
          h = stubHandle
      run h anyAuthUser name `shouldThrow` isQueryException

stubTag :: Tag
stubTag = Tag {tagName = "q", tagId = TagId 1}

stubHandle :: Handle IO
stubHandle =
  Handle
    { hCreateTagNamed = \_ -> pure stubTag
    , hFindTagByName = \_ -> pure Nothing
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
