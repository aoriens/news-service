module Core.Interactor.CreateCategorySpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Authorization
import Core.Authorization.Test
import Core.Category
import Core.Interactor.CreateCategory
import Core.Stubs
import Data.List.NonEmpty
import Test.AsyncExpectation
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    itShouldAuthorizeBeforeOperation AdminPermission $ \authUser authorizationHandle onSuccess -> do
      let parentId = Just $ CategoryId 0
          names = "a" :| ["b"]
          h =
            stubHandle
              { hCreateCategory = \_ _ -> onSuccess >> pure (Right stubCategory)
              , hAuthorizationHandle = authorizationHandle
              }
      void $ run h authUser parentId names
    it "should pass names and parentId to the gateway in a normal case" $ do
      let expectedParentId = Just $ CategoryId 2
          expectedNames = "a" :| []
      shouldPassValue (expectedParentId, expectedNames) "hCreateCategory" $ \onSuccess -> do
        let h =
              stubHandle
                { hCreateCategory =
                    \parentId names -> do
                      onSuccess (parentId, names)
                      pure $ Right stubCategory
                }
        void $ run h someAuthUser expectedParentId expectedNames
    it
      "should return a category returned from the gateway if created successfully" $ do
      let parentId = Just $ CategoryId 0
          names = "a" :| ["b"]
          expectedResult =
            Right
              stubCategory {categoryName = "pascal", categoryId = CategoryId 6}
          h = stubHandle {hCreateCategory = \_ _ -> pure expectedResult}
      r <- run h someAuthUser parentId names
      r `shouldBe` expectedResult
    it
      "should return UnknownParentCategoryId if the gateway returns CCFUnknownParentCategoryId" $ do
      let parentId = Just $ CategoryId 0
          names = "a" :| ["b"]
          h =
            stubHandle
              {hCreateCategory = \_ _ -> pure $ Left CCFUnknownParentCategoryId}
      r <- run h someAuthUser parentId names
      r `shouldBe` Left UnknownParentCategoryId
    it
      "should return IncorrectParameter if the least significant category name is empty" $ do
      let parentId = Nothing
          names = "" :| []
          h = stubHandle {hCreateCategory = \_ _ -> pure $ Right stubCategory}
      r <- run h someAuthUser parentId names
      r `shouldSatisfy` \case
        Left (IncorrectParameter _) -> True
        _ -> False
    it
      "should return IncorrectParameter if a non-least significant category name is empty" $ do
      let parentId = Nothing
          names = "nonempty" :| [""]
          h = stubHandle {hCreateCategory = \_ _ -> pure $ Right stubCategory}
      r <- run h someAuthUser parentId names
      r `shouldSatisfy` \case
        Left (IncorrectParameter _) -> True
        _ -> False

stubHandle :: Handle IO
stubHandle =
  Handle
    { hCreateCategory = undefined
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
