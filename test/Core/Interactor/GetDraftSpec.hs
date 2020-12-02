module Core.Interactor.GetDraftSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
import Core.Category
import Core.Deletable
import Core.Interactor.GetDraft
import Core.News
import Core.User
import qualified Data.HashSet as Set
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    describe "check the user for the draft authorship" $ do
      let draftAuthorId = AuthorId 1
          expectedPerm = AuthorshipPermission draftAuthorId
          draft = stubDraft {nvAuthor = stubAuthor {authorId = draftAuthorId}}
      itShouldAuthorizeBeforeOperation expectedPerm $ \authUser authorizationHandle onSuccess -> do
        let h =
              defaultHandle
                { hGetDraft = \_ -> pure $ Just draft
                , hAuthorizationHandle = authorizationHandle
                }
        void $ run h authUser stubDraftId
        onSuccess
    it "should return hGetDraft result if authorization succeeds" $ do
      let expectedDraft = Just stubDraft {nvId = NewsVersionId 1}
          h = defaultHandle {hGetDraft = const $ pure expectedDraft}
      draft <- run h someAuthUser stubDraftId
      draft `shouldBe` expectedDraft
    it "should pass draft id to hGetDraft" $ do
      ref <- newIORef []
      let expectedDraftId = NewsVersionId 1
          h =
            defaultHandle
              {hGetDraft = \nvId -> modifyIORef' ref (nvId :) >> pure Nothing}
      _ <- run h someAuthUser expectedDraftId
      readIORef ref `shouldReturn` [expectedDraftId]

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetDraft = const $ pure Nothing
    , hAuthorizationHandle = noOpAuthorizationHandle
    }

stubDraftId :: NewsVersionId
stubDraftId = NewsVersionId 0

stubDraft :: NewsVersion
stubDraft =
  NewsVersion
    { nvId = NewsVersionId 0
    , nvTitle = "1"
    , nvText = "2"
    , nvAuthor = stubAuthor
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

stubAuthor :: Author
stubAuthor =
  Author
    { authorId = AuthorId 0
    , authorUser =
        Existing
          User
            { userId = UserId 0
            , userFirstName = Nothing
            , userLastName = ""
            , userAvatarId = Nothing
            , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
            , userIsAdmin = False
            }
    , authorDescription = ""
    }
