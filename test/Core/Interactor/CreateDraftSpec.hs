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
import Core.Tag
import Core.User
import qualified Data.HashSet as Set
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    let creatorId = AuthorId 6
    itShouldAuthorizeBeforeOperation (AuthorshipPermission creatorId) $ \authUser authorizationHandle onSuccess -> do
      let h =
            stubHandle
              { hCreateNewsVersion =
                  \_ -> onSuccess >> pure (Right stubNewsVersion)
              , hAuthorizationHandle = authorizationHandle
              }
          request = stubRequest {cdAuthorId = Just creatorId}
      _ <- run h authUser request
      pure ()
    it "should pass author id from request in the AuthorshipPermission" $ do
      let aid = AuthorId 2
          expectedPermission = AuthorshipPermission aid
          h =
            stubHandle
              { hCreateNewsVersion = \_ -> pure $ Right stubNewsVersion
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
              , cdCategoryId = CategoryId 1
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
              { hCreateNewsVersion =
                  \cmd -> do
                    writeIORef acceptedCommandRef (Just cmd)
                    pure $ Right stubNewsVersion
              }
      _ <- run h someAuthUser request
      acceptedCommand <- readIORef acceptedCommandRef
      cnvTitle <$> acceptedCommand `shouldBe` Just (cdTitle request)
      cnvText <$> acceptedCommand `shouldBe` Just (cdText request)
      cnvAuthorId <$> acceptedCommand `shouldBe` cdAuthorId request
      cnvCategoryId <$> acceptedCommand `shouldBe` Just (cdCategoryId request)
      cnvMainPhoto <$> acceptedCommand `shouldBe` Just (cdMainPhoto request)
      cnvAdditionalPhotos <$>
        acceptedCommand `shouldBe` Just (cdAdditionalPhotos request)
      cnvTagIds <$> acceptedCommand `shouldBe` Just (cdTagIds request)
    it "should pass NewsVersion got from hCreateNewsVersion" $ do
      let request = stubRequest
          expectedVersion = stubNewsVersion {nvId = NewsVersionId 2}
          h =
            stubHandle {hCreateNewsVersion = \_ -> pure $ Right expectedVersion}
      version <- run h someAuthUser request
      version `shouldBe` expectedVersion
    it
      "should throw DependentEntitiesNotFoundException if hCreateNewsVersion returned GUnknownEntityId" $ do
      let request = stubRequest
          ids = [toEntityId $ UserId 1]
          h =
            stubHandle
              {hCreateNewsVersion = \_ -> pure . Left $ GUnknownEntityId ids}
      result <- try $ run h someAuthUser request
      result `shouldBe` Left (DependentEntitiesNotFoundException ids)
    it "should pass main photo to hRejectDisallowedImage if it's Right Image" $ do
      passedPhotosRef <- newIORef []
      let photo = stubImage {imageData = "q"}
          request =
            stubRequest
              {cdMainPhoto = Just $ Right photo, cdAdditionalPhotos = []}
          h =
            stubHandle
              { hRejectDisallowedImage =
                  \img -> modifyIORef' passedPhotosRef (img :)
              }
      _ <- run h someAuthUser request
      readIORef passedPhotosRef `shouldReturn` [photo]
    it
      "should pass all additional photos that are Right Image to hRejectDisallowedImage" $ do
      passedPhotosRef <- newIORef []
      let rightImages =
            [stubImage {imageData = "1"}, stubImage {imageData = "2"}]
          photos = map Right rightImages ++ [Left $ ImageId 3]
          request =
            stubRequest {cdMainPhoto = Nothing, cdAdditionalPhotos = photos}
          h =
            stubHandle
              { hRejectDisallowedImage =
                  \img -> modifyIORef' passedPhotosRef (img :)
              }
      _ <- run h someAuthUser request
      passedPhotos <- readIORef passedPhotosRef
      passedPhotos `shouldMatchList` rightImages
    it
      "should not invoke hRejectDisallowedImage if neither main nor additional photos are Right Image" $ do
      let request =
            stubRequest
              { cdMainPhoto = Just . Left $ ImageId 1
              , cdAdditionalPhotos = [Left $ ImageId 2]
              }
          h =
            stubHandle
              { hRejectDisallowedImage =
                  \img -> error $ "Must not invoke with parameter " ++ show img
              }
      _ <- run h someAuthUser request
      pure ()
    it
      "should not invoke hCreateNewsVersion if hRejectDisallowedImage threw an exception on main photo" $ do
      let expectedError = "expected"
          request = stubRequest {cdMainPhoto = Just $ Right stubImage}
          h =
            stubHandle
              { hRejectDisallowedImage = \_ -> error expectedError
              , hCreateNewsVersion = \_ -> error "Must not invoke"
              }
      run h someAuthUser request `shouldThrow` errorCall expectedError
    it
      "should not invoke hCreateNewsVersion if hRejectDisallowedImage threw an exception on an additional photo" $ do
      let expectedError = "expected"
          request =
            stubRequest
              {cdMainPhoto = Nothing, cdAdditionalPhotos = [Right stubImage]}
          h =
            stubHandle
              { hRejectDisallowedImage = \_ -> error expectedError
              , hCreateNewsVersion = \_ -> error "Must not invoke"
              }
      run h someAuthUser request `shouldThrow` errorCall expectedError
    it
      "should pass authorId to hCreateNewsVersion from hGetAuthorIdByUserIdIfExactlyOne if CreateDraftRequest has no authorId" $ do
      passedAuthorsId <- newIORef []
      let request = stubRequest {cdAuthorId = Nothing}
          expectedAuthorId = AuthorId 1
          authUser = IdentifiedUser (UserId 1) False []
          h =
            stubHandle
              { hGetAuthorIdByUserIdIfExactlyOne =
                  \_ -> pure $ Just expectedAuthorId
              , hCreateNewsVersion =
                  \cmd -> do
                    modifyIORef' passedAuthorsId (cnvAuthorId cmd :)
                    pure (Right stubNewsVersion)
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
                    perm == AuthorshipPermission expectedAuthorId
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
      "should throw UserNotIdentifiedException if CreateDraftRequest has no author and authenticate returns AnonymousUser" $ do
      let request = stubRequest {cdAuthorId = Nothing}
          authUser = AnonymousUser
          h =
            stubHandle
              { hGetAuthorIdByUserIdIfExactlyOne =
                  \_ -> pure $ Just $ AuthorId 1
              }
      run h authUser request `shouldThrow` isUserNotIdentifiedException

stubNewsVersion :: NewsVersion
stubNewsVersion =
  NewsVersion
    { nvId = NewsVersionId 999
    , nvTitle = "1"
    , nvText = "2"
    , nvAuthor =
        Author
          { authorId = AuthorId 999
          , authorUser =
              Existing
                User
                  { userId = UserId 12
                  , userFirstName = Nothing
                  , userLastName = ""
                  , userAvatarId = Nothing
                  , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
                  , userIsAdmin = False
                  }
          , authorDescription = "Yo"
          }
    , nvCategory =
        Category
          { categoryId = CategoryId 1
          , categoryName = ""
          , categoryParent = Nothing
          }
    , nvTags = Set.empty
    , nvAdditionalPhotoIds = Set.empty
    , nvMainPhotoId = Nothing
    }

stubRequest :: CreateDraftRequest
stubRequest =
  CreateDraftRequest
    { cdTitle = "one"
    , cdText = "two"
    , cdAuthorId = Just $ AuthorId 888
    , cdCategoryId = CategoryId 888
    , cdMainPhoto = Nothing
    , cdAdditionalPhotos = []
    , cdTagIds = Set.empty
    }

stubHandle :: Handle IO
stubHandle =
  Handle
    { hCreateNewsVersion = \_ -> pure $ Right stubNewsVersion
    , hGetAuthorIdByUserIdIfExactlyOne = \_ -> pure Nothing
    , hAuthorizationHandle = noOpAuthorizationHandle
    , hRejectDisallowedImage = \_ -> pure ()
    }

stubImage :: Image
stubImage = Image {imageContentType = "", imageData = ""}
