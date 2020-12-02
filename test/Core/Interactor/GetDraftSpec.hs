module Core.Interactor.GetDraftSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Category
import Core.Deletable
import Core.Exception
import Core.Interactor.GetDraft
import Core.News
import Core.User
import qualified Data.HashSet as Set
import Data.List
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it
      "should return an existing draft for the given id if the user is its author" $ do
      let draftId = NewsVersionId 1
          authorId = AuthorId 2
          draft = draftWithIdAndAuthorId draftId authorId
          authUser = IdentifiedUser (UserId 0) False [authorId]
          h = handleWithItems [draft]
      r <- run h authUser draftId
      r `shouldBe` Just draft
    it
      "should throw NoPermissionException for an existing draft if the user is not its author" $ do
      let draftId = NewsVersionId 1
          draft = draftWithIdAndAuthorId draftId (AuthorId 2)
          authUser = IdentifiedUser (UserId 0) False [AuthorId 3]
          h = handleWithItems [draft]
      run h authUser draftId `shouldThrow` isNoPermissionException
    it
      "should throw NoPermissionException for an existing draft if the user is not its author, but is an admin" $ do
      let draftId = NewsVersionId 1
          draft = draftWithIdAndAuthorId draftId (AuthorId 2)
          authUser = IdentifiedUser (UserId 0) True [AuthorId 3]
          h = handleWithItems [draft]
      run h authUser draftId `shouldThrow` isNoPermissionException
    it "should return Nothing if no draft is found for the given id" $ do
      let requestedDraftId = NewsVersionId 1
          draft = draftWithIdAndAuthorId (NewsVersionId 2) (AuthorId 3)
          h = handleWithItems [draft]
      r <- run h someNonAdminUser requestedDraftId
      r `shouldBe` Nothing

handleWithItems :: Applicative m => [NewsVersion] -> Handle m
handleWithItems items =
  Handle {hGetDraft = \targetId -> pure $ find (\v -> targetId == nvId v) items}

draftWithIdAndAuthorId :: NewsVersionId -> AuthorId -> NewsVersion
draftWithIdAndAuthorId nvId authorId =
  stubDraft {nvId, nvAuthor = stubAuthor {authorId}}

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
