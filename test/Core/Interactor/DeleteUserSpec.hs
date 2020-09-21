module Core.Interactor.DeleteUserSpec
  ( spec
  ) where

import Control.Exception
import Core.Authentication.Test
import Core.Author
import qualified Core.Authorization.Impl
import Core.EntityId
import Core.Exception
import Core.Interactor.DeleteUser
import Core.Pagination
import Core.User
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldRequireAdminPermission $ \credentials authHandle onSuccess -> do
      let uid = UserId 1
          h =
            stubHandle
              { hDeleteUser = \_ _ -> onSuccess >> pure (Right ())
              , hAuthHandle = authHandle
              }
      run h credentials uid
    it
      "should throw DependentEntitiesPreventDeletionException if \
       \the gateway returned Left DependentEntitiesPreventDeletion" $ do
      let uid = UserId 1
          authorIds = [AuthorEntityId $ AuthorId 0]
          h =
            stubHandle
              { hDeleteUser =
                  \_ _ ->
                    pure . Left $ DependentEntitiesPreventDeletion authorIds
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      r <- try $ run h noCredentials uid
      r `shouldBe`
        Left
          (DependentEntitiesPreventDeletionException
             (UserEntityId uid)
             authorIds)
    it
      "should throw EntityNotFoundException if \
       \the gateway returned Left UnknownUser" $ do
      let uid = UserId 1
          h =
            stubHandle
              { hDeleteUser = \_ _ -> pure $ Left UnknownUser
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      r <- try $ run h noCredentials uid
      r `shouldBe` Left (EntityNotFoundException $ UserEntityId uid)
    it "should pass the UserId argument to the gateway delete command" $ do
      passedUserId <- newIORef undefined
      let expectedUid = UserId 8
          h =
            stubHandle
              { hDeleteUser =
                  \uid _ -> writeIORef passedUserId uid >> pure (Right ())
              }
      run h noCredentials expectedUid
      readIORef passedUserId `shouldReturn` expectedUid

stubHandle :: Handle IO
stubHandle =
  Handle
    { hDeleteUser = \_ _ -> pure (Right ())
    , hAuthHandle = stubAuthHandleReturningAdminUser
    , hDefaultEntityListRange = PageSpec (PageOffset 0) (PageLimit 0)
    , hAuthorizationHandle = Core.Authorization.Impl.new
    }
