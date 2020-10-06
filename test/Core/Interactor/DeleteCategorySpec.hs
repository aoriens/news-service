module Core.Interactor.DeleteCategorySpec
  ( spec
  ) where

import Control.Exception
import Core.Authentication.Test
import Core.Authorization
import Core.Authorization.Test
import Core.Category
import Core.EntityId
import Core.Exception
import Core.Interactor.DeleteCategory
import Core.Pagination
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldAuthenticateAndAuthorizeBeforeOperation AdminPermission $ \credentials authenticationHandle authorizationHandle onSuccess -> do
      let catId = CategoryId 1
          h =
            stubHandle
              { hDeleteCategory = \_ _ -> onSuccess >> pure (Right ())
              , hAuthenticationHandle = authenticationHandle
              , hAuthorizationHandle = authorizationHandle
              }
      run h credentials catId
    it
      "should throw DependentEntitiesPreventDeletionException if \
       \the gateway returned Left DependentEntitiesPreventDeletion" $ do
      let catId = CategoryId 1
          childCatIds = [CategoryEntityId $ CategoryId 2]
          h =
            stubHandle
              { hDeleteCategory =
                  \_ _ ->
                    pure . Left $ DependentEntitiesPreventDeletion childCatIds
              }
      r <- try $ run h noCredentials catId
      r `shouldBe`
        Left
          (DependentEntitiesPreventDeletionException
             (CategoryEntityId catId)
             childCatIds)
    it
      "should throw EntityNotFoundException if \
       \the gateway returned Left UnknownCategory" $ do
      let catId = CategoryId 1
          h = stubHandle {hDeleteCategory = \_ _ -> pure $ Left UnknownCategory}
      r <- try $ run h noCredentials catId
      r `shouldBe` Left (EntityNotFoundException $ CategoryEntityId catId)
    it "should pass the CategoryId argument to the gateway delete command" $ do
      passedCategoryId <- newIORef undefined
      let expectedCatId = CategoryId 8
          h =
            stubHandle
              { hDeleteCategory =
                  \catId _ ->
                    writeIORef passedCategoryId catId >> pure (Right ())
              }
      run h noCredentials expectedCatId
      readIORef passedCategoryId `shouldReturn` expectedCatId

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteCategory = \_ _ -> pure (Right ())
    , hAuthenticationHandle = noOpAuthenticationHandle
    , hDefaultEntityListRange = PageSpec (PageOffset 0) (PageLimit 0)
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
