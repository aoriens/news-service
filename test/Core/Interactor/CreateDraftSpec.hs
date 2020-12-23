module Core.Interactor.CreateDraftSpec
  ( spec
  ) where

import Control.Exception
import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
import Core.Category
import Core.Deletable
import Core.EntityId
import Core.Exception
import Core.Image
import Core.Interactor.CreateDraft
import Core.News
import Core.Stubs
import Core.Tag
import Core.User
import qualified Data.HashSet as Set
import Data.IORef
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    let creatorId = AuthorId 6
    itShouldAuthorizeBeforeOperation (AuthorshipPermission $ Existing creatorId) $ \authUser authorizationHandle onSuccess -> do
      let h =
            stubHandle
              { hCreateDraft = \_ -> onSuccess >> pure (Right stubDraft)
              , hAuthorizationHandle = authorizationHandle
              }
          request = stubRequest {cdAuthorId = Just creatorId}
      _ <- run h authUser request
      pure ()
    it "should pass author id from request in the AuthorshipPermission" $ do
      let aid = AuthorId 2
          expectedPermission = AuthorshipPermission $ Existing aid
          h =
            stubHandle
              { hCreateDraft = \_ -> pure $ Right stubDraft
              , hAuthorizationHandle =
                  AuthorizationHandle
                    {hHasPermission = \perm _ -> perm == expectedPermission}
              }
          request = stubRequest {cdAuthorId = Just aid}
      _ <- run h someAuthUser request
      pure ()
    it
      "should pass title, text, author id, category id, main photo, other photos, and tag ids to the gateway" $ do
      acceptedCommandRef <- newIORef Nothing
      let request =
            CreateDraftRequest
              { cdTitle = "title"
              , cdText = "text"
              , cdAuthorId = Just $ AuthorId 1
              , cdCategoryId = Just $ CategoryId 1
              , cdMainPhoto = Just . Left $ ImageId 1
              , cdAdditionalPhotos =
                  [ Left $ ImageId 2
                  , Right
                      Image
                        { imageContentType = "image/sometype"
                        , imageData = "imagedata"
                        }
                  ]
              , cdTagIds = Set.fromList [TagId 1, TagId 2]
              }
          h =
            stubHandle
              { hCreateDraft =
                  \cmd -> do
                    writeIORef acceptedCommandRef (Just cmd)
                    pure $ Right stubDraft
              }
      _ <- run h someAuthUser request
      acceptedCommand <- readIORef acceptedCommandRef
      cdcTitle <$> acceptedCommand `shouldBe` Just (cdTitle request)
      cdcText <$> acceptedCommand `shouldBe` Just (cdText request)
      cdcAuthorId <$> acceptedCommand `shouldBe` cdAuthorId request
      cdcCategoryId <$> acceptedCommand `shouldBe` Just (cdCategoryId request)
      cdcMainPhoto <$> acceptedCommand `shouldBe` Just (cdMainPhoto request)
      cdcAdditionalPhotos <$>
        acceptedCommand `shouldBe` Just (cdAdditionalPhotos request)
      cdcTagIds <$> acceptedCommand `shouldBe` Just (cdTagIds request)
    it "should pass NewsVersion got from hCreateDraft" $ do
      let request = stubRequest
          expectedDraft = stubDraft {draftId = DraftId 2}
          h = stubHandle {hCreateDraft = \_ -> pure $ Right expectedDraft}
      version <- run h someAuthUser request
      version `shouldBe` expectedDraft
    it
      "should throw DependentEntitiesNotFoundException if hCreateDraft returned CDUnknownEntityId" $ do
      let request = stubRequest
          ids = [toEntityId $ UserId 1]
          h =
            stubHandle
              {hCreateDraft = \_ -> pure . Left $ CDUnknownEntityId ids}
      result <- try $ run h someAuthUser request
      result `shouldBe` Left (DependentEntitiesNotFoundException ids)
    it "should pass main photo to hRejectImageIfDisallowed if it's Right Image" $ do
      passedPhotosRef <- newIORef []
      let photo = stubImage {imageData = "q"}
          request =
            stubRequest
              {cdMainPhoto = Just $ Right photo, cdAdditionalPhotos = []}
          h =
            stubHandle
              { hRejectImageIfDisallowed =
                  \img -> modifyIORef' passedPhotosRef (img :)
              }
      _ <- run h someAuthUser request
      readIORef passedPhotosRef `shouldReturn` [photo]
    it
      "should pass all additional photos that are Right Image to hRejectImageIfDisallowed" $ do
      passedPhotosRef <- newIORef []
      let rightImages =
            [stubImage {imageData = "1"}, stubImage {imageData = "2"}]
          photos = map Right rightImages ++ [Left $ ImageId 3]
          request =
            stubRequest {cdMainPhoto = Nothing, cdAdditionalPhotos = photos}
          h =
            stubHandle
              { hRejectImageIfDisallowed =
                  \img -> modifyIORef' passedPhotosRef (img :)
              }
      _ <- run h someAuthUser request
      passedPhotos <- readIORef passedPhotosRef
      passedPhotos `shouldMatchList` rightImages
    it
      "should not invoke hRejectImageIfDisallowed if neither main nor additional photos are Right Image" $ do
      let request =
            stubRequest
              { cdMainPhoto = Just . Left $ ImageId 1
              , cdAdditionalPhotos = [Left $ ImageId 2]
              }
          h =
            stubHandle
              { hRejectImageIfDisallowed =
                  \img -> error $ "Must not invoke with parameter " ++ show img
              }
      _ <- run h someAuthUser request
      pure ()
    it
      "should not invoke hCreateDraft if hRejectImageIfDisallowed threw an exception on main photo" $ do
      let expectedError = "expected"
          request = stubRequest {cdMainPhoto = Just $ Right stubImage}
          h =
            stubHandle
              { hRejectImageIfDisallowed = \_ -> error expectedError
              , hCreateDraft = \_ -> error "Must not invoke"
              }
      run h someAuthUser request `shouldThrow` errorCall expectedError
    it
      "should not invoke hCreateDraft if hRejectImageIfDisallowed threw an exception on an additional photo" $ do
      let expectedError = "expected"
          request =
            stubRequest
              {cdMainPhoto = Nothing, cdAdditionalPhotos = [Right stubImage]}
          h =
            stubHandle
              { hRejectImageIfDisallowed = \_ -> error expectedError
              , hCreateDraft = \_ -> error "Must not invoke"
              }
      run h someAuthUser request `shouldThrow` errorCall expectedError
    it
      "should pass authorId to hCreateDraft from hGetAuthorIdByUserIdIfExactlyOne if CreateDraftRequest has no authorId" $ do
      passedAuthorsId <- newIORef []
      let request = stubRequest {cdAuthorId = Nothing}
          expectedAuthorId = AuthorId 1
          authUser = IdentifiedUser (UserId 1) False []
          h =
            stubHandle
              { hGetAuthorIdByUserIdIfExactlyOne =
                  \_ -> pure $ Just expectedAuthorId
              , hCreateDraft =
                  \cmd -> do
                    modifyIORef' passedAuthorsId (cdcAuthorId cmd :)
                    pure (Right stubDraft)
              }
      _ <- run h authUser request
      readIORef passedAuthorsId `shouldReturn` [expectedAuthorId]
    it
      "should pass authorId to authorization from hGetAuthorIdByUserIdIfExactlyOne if CreateDraftRequest has no authorId" $ do
      let request = stubRequest {cdAuthorId = Nothing}
          expectedAuthorId = AuthorId 1
          authUser = IdentifiedUser (UserId 1) False []
          h =
            stubHandle
              { hGetAuthorIdByUserIdIfExactlyOne =
                  \_ -> pure $ Just expectedAuthorId
              , hAuthorizationHandle =
                  AuthorizationHandle $ \perm _ ->
                    perm == AuthorshipPermission (Existing expectedAuthorId)
              }
      _ <- run h authUser request
      pure ()
    it
      "should pass UserId from AuthenticatedUser from authenticate to hGetAuthorIdByUserIdIfExactlyOne if CreateDraftRequest has no authorId" $ do
      passedUserIds <- newIORef []
      let request = stubRequest {cdAuthorId = Nothing}
          expectedUserId = UserId 1
          authUser = IdentifiedUser expectedUserId False []
          h =
            stubHandle
              { hGetAuthorIdByUserIdIfExactlyOne =
                  \userId' -> do
                    modifyIORef' passedUserIds (userId' :)
                    pure $ Just $ AuthorId 2
              }
      _ <- run h authUser request
      readIORef passedUserIds `shouldReturn` [expectedUserId]
    it
      "should not invoke hGetAuthorsOfUser if author is specified in CreateDraftRequest" $ do
      invoked <- newIORef False
      let request = stubRequest {cdAuthorId = Just $ AuthorId 1}
          h =
            stubHandle
              { hGetAuthorIdByUserIdIfExactlyOne =
                  \_ -> modifyIORef' invoked succ >> pure Nothing
              }
      _ <- run h someAuthUser request
      readIORef invoked `shouldReturn` False
    it
      "should throw QueryException if CreateDraftRequest has no author and hGetAuthorIdByUserIdIfExactlyOne returns Nothing" $ do
      let request = stubRequest {cdAuthorId = Nothing}
          authUser = IdentifiedUser (UserId 1) False []
          h = stubHandle {hGetAuthorIdByUserIdIfExactlyOne = \_ -> pure Nothing}
      run h authUser request `shouldThrow` isQueryException
    it
      "should throw AuthenticationRequiredException if CreateDraftRequest has no author and authenticate returns AnonymousUser" $ do
      let request = stubRequest {cdAuthorId = Nothing}
          authUser = AnonymousUser
          h =
            stubHandle
              { hGetAuthorIdByUserIdIfExactlyOne =
                  \_ -> pure $ Just $ AuthorId 1
              }
      run h authUser request `shouldThrow` (== AuthenticationRequiredException)

stubRequest :: CreateDraftRequest
stubRequest =
  CreateDraftRequest
    { cdTitle = "one"
    , cdText = "two"
    , cdAuthorId = Just $ AuthorId 888
    , cdCategoryId = Nothing
    , cdMainPhoto = Nothing
    , cdAdditionalPhotos = []
    , cdTagIds = Set.empty
    }

stubHandle :: Handle IO
stubHandle =
  Handle
    { hCreateDraft = \_ -> pure $ Right stubDraft
    , hGetAuthorIdByUserIdIfExactlyOne = \_ -> pure Nothing
    , hAuthorizationHandle = noOpAuthorizationHandle
    , hRejectImageIfDisallowed = \_ -> pure ()
    }

stubImage :: Image
stubImage = Image {imageContentType = "", imageData = ""}
