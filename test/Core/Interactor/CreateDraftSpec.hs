module Core.Interactor.CreateDraftSpec
  ( spec
  ) where

import Control.Exception
import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
import Core.Category
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
    itShouldAuthenticateAndAuthorizeBeforeOperation
      (AuthorshipPermission creatorId) $ \credentials authenticationHandle authorizationHandle onSuccess -> do
      let h =
            stubHandle
              { hCreateNewsVersion =
                  \_ -> onSuccess >> pure (Right stubNewsVersion)
              , hAuthenticationHandle = authenticationHandle
              , hAuthorizationHandle = authorizationHandle
              }
          request = stubRequest {cdAuthorId = creatorId}
      _ <- run h credentials request
      pure ()
    it "should pass author id from request in the AuthorshipPermission" $ do
      let aid = AuthorId 2
          expectedPermission = AuthorshipPermission aid
          h =
            stubHandle
              { hCreateNewsVersion = \_ -> pure $ Right stubNewsVersion
              , hAuthenticationHandle = noOpAuthenticationHandle
              , hAuthorizationHandle =
                  AuthorizationHandle
                    {hHasPermission = \perm _ -> perm == expectedPermission}
              }
          request = stubRequest {cdAuthorId = aid}
      _ <- run h noCredentials request
      pure ()
    it
      "should pass title, text, author id, category id, main photo, other photos, and tag ids to the gateway" $ do
      acceptedCommandRef <- newIORef Nothing
      let request =
            CreateDraftRequest
              { cdTitle = "title"
              , cdText = "text"
              , cdAuthorId = AuthorId 1
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
      _ <- run h noCredentials request
      acceptedCommand <- readIORef acceptedCommandRef
      cnvTitle <$> acceptedCommand `shouldBe` Just (cdTitle request)
      cnvText <$> acceptedCommand `shouldBe` Just (cdText request)
      cnvAuthorId <$> acceptedCommand `shouldBe` Just (cdAuthorId request)
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
      version <- run h noCredentials request
      version `shouldBe` expectedVersion
    it
      "should throw DependentEntitiesNotFoundException if hCreateNewsVersion returned GUnknownEntityId" $ do
      let request = stubRequest
          ids = [toEntityId $ UserId 1]
          h =
            stubHandle
              {hCreateNewsVersion = \_ -> pure . Left $ GUnknownEntityId ids}
      result <- try $ run h noCredentials request
      result `shouldBe` Left (DependentEntitiesNotFoundException ids)

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
    , cdAuthorId = AuthorId 888
    , cdCategoryId = CategoryId 888
    , cdMainPhoto = Nothing
    , cdAdditionalPhotos = []
    , cdTagIds = Set.empty
    }

stubHandle :: Handle IO
stubHandle =
  Handle
    { hCreateNewsVersion = undefined
    , hAuthenticationHandle = noOpAuthenticationHandle
    , hAuthorizationHandle = noOpAuthorizationHandle
    }
