module Core.Interactor.GetDraftSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Interactor.GetDraft
import Core.News
import Core.Stubs
import Core.User
import Data.List
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it
      "should return an existing draft for the given id if the user is its author" $ do
      let draftId = DraftId 1
          authorId = AuthorId 2
          draft = draftWithIdAndAuthorId draftId authorId
          authUser = IdentifiedUser (UserId 0) False [authorId]
          h = handleWithItems [draft]
      r <- run h authUser draftId
      r `shouldBe` Just draft
    it
      "should throw NoPermissionException for an existing draft if the user is not its author" $ do
      let draftId = DraftId 1
          draft = draftWithIdAndAuthorId draftId (AuthorId 2)
          authUser = IdentifiedUser (UserId 0) False [AuthorId 3]
          h = handleWithItems [draft]
      run h authUser draftId `shouldThrow` isNoPermissionException
    it
      "should throw NoPermissionException for an existing draft if the user is not its author, but is an admin" $ do
      let draftId = DraftId 1
          draft = draftWithIdAndAuthorId draftId (AuthorId 2)
          authUser = IdentifiedUser (UserId 0) True [AuthorId 3]
          h = handleWithItems [draft]
      run h authUser draftId `shouldThrow` isNoPermissionException
    it "should return Nothing if no draft is found for the given id" $ do
      let requestedDraftId = DraftId 1
          draft = draftWithIdAndAuthorId (DraftId 2) (AuthorId 3)
          h = handleWithItems [draft]
      r <- run h someNonAdminUser requestedDraftId
      r `shouldBe` Nothing
    it "should return Nothing if the draft is found, but its author is deleted" $ do
      let draftId = DraftId 1
          draft =
            stubDraft
              {draftId, draftContent = stubNewsVersion {nvAuthor = Deleted}}
          h = handleWithItems [draft]
      r <- run h someAdminUser draftId
      r `shouldBe` Nothing

handleWithItems :: Applicative m => [Draft] -> Handle m
handleWithItems items =
  Handle
    {hGetDraft = \targetId -> pure $ find (\v -> targetId == draftId v) items}

draftWithIdAndAuthorId :: DraftId -> AuthorId -> Draft
draftWithIdAndAuthorId draftId authorId =
  stubDraft
    { draftId
    , draftContent = stubNewsVersion {nvAuthor = Existing stubAuthor {authorId}}
    }
