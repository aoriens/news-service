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
    itShouldAuthorizeBeforeOperation AdminPermission $ \authUser authorizationHandle onSuccess -> do
      let catId = CategoryId 1
          h =
            stubHandle
              { hDeleteCategory = \_ _ -> onSuccess >> pure (Right ())
              , hAuthorizationHandle = authorizationHandle
              }
      run h authUser catId
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
      r <- try $ run h someAuthUser catId
      r `shouldBe`
        Left
          (DependentEntitiesPreventDeletionException
             (CategoryEntityId catId)
             childCatIds)
    it
      "should throw RequestedEntityNotFoundException if \
       \the gateway returned Left UnknownCategory" $ do
      let catId = CategoryId 1
          h = stubHandle {hDeleteCategory = \_ _ -> pure $ Left UnknownCategory}
      r <- try $ run h someAuthUser catId
      r `shouldBe`
        Left (RequestedEntityNotFoundException $ CategoryEntityId catId)
    it "should pass the CategoryId argument to the gateway delete command" $ do
      passedCategoryId <- newIORef undefined
      let expectedCatId = CategoryId 8
          h =
            stubHandle
              { hDeleteCategory =
                  \catId _ ->
                    writeIORef passedCategoryId catId >> pure (Right ())
              }
      run h someAuthUser expectedCatId
      readIORef passedCategoryId `shouldReturn` expectedCatId

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteCategory = \_ _ -> pure (Right ())
    , hDefaultEntityListRange = PageSpec (PageOffset 0) (PageLimit 0)
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
