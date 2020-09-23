module Core.Interactor.CreateCategorySpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Authorization
import Core.Authorization.Test
import Core.Category
import Core.Interactor.CreateCategory
import Data.List.NonEmpty
import Test.AsyncExpectation
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    itShouldAuthenticateAndAuthorizeBeforeOperation AdminPermission $ \credentials authHandle authorizationHandle onSuccess -> do
      let parentId = Just $ CategoryId 0
          names = "a" :| ["b"]
          h =
            stubHandle
              { hCreateCategory = \_ _ -> onSuccess >> pure (Right stubCategory)
              , hAuthHandle = authHandle
              , hAuthorizationHandle = authorizationHandle
              }
      void $ run h credentials parentId names
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
        void $ run h noCredentials expectedParentId expectedNames
    it
      "should return a category returned from the gateway if created successfully" $ do
      let parentId = Just $ CategoryId 0
          names = "a" :| ["b"]
          expectedResult =
            Right
              stubCategory {categoryName = "pascal", categoryId = CategoryId 6}
          h = stubHandle {hCreateCategory = \_ _ -> pure expectedResult}
      r <- run h noCredentials parentId names
      r `shouldBe` expectedResult
    it "should return failure returned from the gateway if any" $ do
      let parentId = Just $ CategoryId 0
          names = "a" :| ["b"]
          expectedResult = Left UnknownParentCategoryId
          h = stubHandle {hCreateCategory = \_ _ -> pure expectedResult}
      r <- run h noCredentials parentId names
      r `shouldBe` expectedResult

stubCategory :: Category
stubCategory =
  Category
    { categoryName = "haskell"
    , categoryId = CategoryId 1
    , categoryParent =
        Just
          Category
            { categoryName = "programming"
            , categoryId = CategoryId 2
            , categoryParent = Nothing
            }
    }

stubHandle :: Handle IO
stubHandle =
  Handle
    { hCreateCategory = undefined
    , hAuthHandle = noOpAuthenticationHandle
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
