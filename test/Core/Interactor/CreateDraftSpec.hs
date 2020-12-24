module Core.Interactor.CreateDraftSpec
  ( spec
  ) where

import Control.Exception
import Core.Authentication
import Core.Author
import Core.Category
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
    it
      "should throw NoPermissionException and should not create a draft if authorId is specified and the user does not own it" $ do
      hasCreatedDraft <- newIORef False
      let unownedAuthorId = AuthorId 1
          ownedAuthorId = AuthorId 2
          request = stubRequest {cdAuthorId = Just unownedAuthorId}
          authUser = IdentifiedUser (UserId 3) False [ownedAuthorId]
          h =
            stubHandle
              { hCreateDraft =
                  \_ ->
                    writeIORef hasCreatedDraft True >> pure (Right stubDraft)
              }
      run h authUser request `shouldThrow` isNoPermissionException
      readIORef hasCreatedDraft `shouldReturn` False
    it
      "should pass title, text, author id, category id, main photo, other photos, and tag ids to the gateway" $ do
      acceptedCommandRef <- newIORef Nothing
      let request =
            CreateDraftRequest
              { cdTitle = "title"
              , cdText = "text"
              , cdAuthorId = Just authorId
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
          authUser = IdentifiedUser (UserId 0) False [authorId]
          authorId = AuthorId 1
      _ <- run h authUser request
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
      let request = stubRequest {cdAuthorId = Just authorId}
          expectedDraft = stubDraft {draftId = DraftId 2}
          h = stubHandle {hCreateDraft = \_ -> pure $ Right expectedDraft}
          authUser = IdentifiedUser (UserId 0) False [authorId]
          authorId = AuthorId 1
      version <- run h authUser request
      version `shouldBe` expectedDraft
    it
      "should throw DependentEntitiesNotFoundException if hCreateDraft returned CDUnknownEntityId" $ do
      let request = stubRequest {cdAuthorId = Just authorId}
          ids = [toEntityId $ UserId 1]
          h =
            stubHandle
              {hCreateDraft = \_ -> pure . Left $ CDUnknownEntityId ids}
          authUser = IdentifiedUser (UserId 0) False [authorId]
          authorId = AuthorId 1
      result <- try $ run h authUser request
      result `shouldBe` Left (DependentEntitiesNotFoundException ids)
    it "should pass main photo to hRejectImageIfDisallowed if it's Right Image" $ do
      passedPhotosRef <- newIORef []
      let photo = stubImage {imageData = "q"}
          request =
            stubRequest
              { cdMainPhoto = Just $ Right photo
              , cdAdditionalPhotos = []
              , cdAuthorId = Just authorId
              }
          h =
            stubHandle
              { hRejectImageIfDisallowed =
                  \img -> modifyIORef' passedPhotosRef (img :)
              }
          authUser = IdentifiedUser (UserId 0) False [authorId]
          authorId = AuthorId 1
      _ <- run h authUser request
      readIORef passedPhotosRef `shouldReturn` [photo]
    it
      "should pass all additional photos that are Right Image to hRejectImageIfDisallowed" $ do
      passedPhotosRef <- newIORef []
      let rightImages =
            [stubImage {imageData = "1"}, stubImage {imageData = "2"}]
          photos = map Right rightImages ++ [Left $ ImageId 3]
          request =
            stubRequest
              { cdMainPhoto = Nothing
              , cdAdditionalPhotos = photos
              , cdAuthorId = Just authorId
              }
          h =
            stubHandle
              { hRejectImageIfDisallowed =
                  \img -> modifyIORef' passedPhotosRef (img :)
              }
          authUser = IdentifiedUser (UserId 0) False [authorId]
          authorId = AuthorId 1
      _ <- run h authUser request
      passedPhotos <- readIORef passedPhotosRef
      passedPhotos `shouldMatchList` rightImages
    it
      "should not invoke hRejectImageIfDisallowed if neither main nor additional photos are Right Image" $ do
      let request =
            stubRequest
              { cdMainPhoto = Just . Left $ ImageId 1
              , cdAdditionalPhotos = [Left $ ImageId 2]
              , cdAuthorId = Just authorId
              }
          h =
            stubHandle
              { hRejectImageIfDisallowed =
                  \img -> error $ "Must not invoke with parameter " ++ show img
              }
          authUser = IdentifiedUser (UserId 0) False [authorId]
          authorId = AuthorId 1
      _ <- run h authUser request
      pure ()
    it
      "should not invoke hCreateDraft if hRejectImageIfDisallowed threw an exception on main photo" $ do
      let expectedError = "expected"
          request =
            stubRequest
              {cdMainPhoto = Just $ Right stubImage, cdAuthorId = Just authorId}
          h =
            stubHandle
              { hRejectImageIfDisallowed = \_ -> error expectedError
              , hCreateDraft = \_ -> error "Must not invoke"
              }
          authUser = IdentifiedUser (UserId 0) False [authorId]
          authorId = AuthorId 1
      run h authUser request `shouldThrow` errorCall expectedError
    it
      "should not invoke hCreateDraft if hRejectImageIfDisallowed threw an exception on an additional photo" $ do
      let expectedError = "expected"
          request =
            stubRequest
              { cdMainPhoto = Nothing
              , cdAdditionalPhotos = [Right stubImage]
              , cdAuthorId = Just authorId
              }
          h =
            stubHandle
              { hRejectImageIfDisallowed = \_ -> error expectedError
              , hCreateDraft = \_ -> error "Must not invoke"
              }
          authUser = IdentifiedUser (UserId 0) False [authorId]
          authorId = AuthorId 1
      run h authUser request `shouldThrow` errorCall expectedError
    it
      "should pass authorId to hCreateDraft from hGetAuthorIdByUserIdIfExactlyOne if CreateDraftRequest has no authorId" $ do
      passedAuthorsId <- newIORef []
      let request = stubRequest {cdAuthorId = Nothing}
          expectedAuthorId = AuthorId 1
          authUser = IdentifiedUser (UserId 1) False [expectedAuthorId]
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
      "should pass UserId from AuthenticatedUser from authenticate to hGetAuthorIdByUserIdIfExactlyOne if CreateDraftRequest has no authorId" $ do
      passedUserIds <- newIORef []
      let request = stubRequest {cdAuthorId = Nothing}
          expectedUserId = UserId 1
          authUser = IdentifiedUser expectedUserId False [authorId]
          authorId = AuthorId 2
          h =
            stubHandle
              { hGetAuthorIdByUserIdIfExactlyOne =
                  \userId' -> do
                    modifyIORef' passedUserIds (userId' :)
                    pure $ Just authorId
              }
      _ <- run h authUser request
      readIORef passedUserIds `shouldReturn` [expectedUserId]
    it
      "should throw IncorrectParameterException if CreateDraftRequest has no author and hGetAuthorIdByUserIdIfExactlyOne returns Nothing" $ do
      let request = stubRequest {cdAuthorId = Nothing}
          authUser = IdentifiedUser (UserId 1) False []
          h = stubHandle {hGetAuthorIdByUserIdIfExactlyOne = \_ -> pure Nothing}
      run h authUser request `shouldThrow` isIncorrectParameterException
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
    , hRejectImageIfDisallowed = \_ -> pure ()
    }

stubImage :: Image
stubImage = Image {imageContentType = "", imageData = ""}
