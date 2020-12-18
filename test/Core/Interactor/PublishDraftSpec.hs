module Core.Interactor.PublishDraftSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Interactor.PublishDraft
import Core.News
import Core.Stubs
import Core.User
import Data.IORef
import Data.IORef.Util
import Data.List
import Data.Maybe
import Data.Time
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should return Left UnknownDraftId if no such draft is found" $ do
      let missingDraftId = DraftId 1
          initialData = storageWithDrafts [draftWithId $ DraftId 2]
      db <- newIORef initialData
      let h = handleWith stubDay db
      r <- run h someAdminUser missingDraftId
      r `shouldBe` Left UnknownDraftId
      readIORef db `shouldReturn` initialData
    it
      "should throw NoPermissionException for an existing draft with the author deleted" $ do
      let draftId = DraftId 1
          initialData = storageWithDrafts [draftWithIdAndDeletedAuthor draftId]
      db <- newIORef initialData
      let h = handleWith stubDay db
      run h someAdminUser draftId `shouldThrow` isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should throw NoPermissionException if the user is not an author of an existing draft" $ do
      let draftId = DraftId 1
          user = IdentifiedUser (UserId 0) False [AuthorId 2]
          initialData =
            storageWithDrafts [draftWithIdAndAuthorId draftId $ AuthorId 3]
      db <- newIORef initialData
      let h = handleWith stubDay db
      run h user draftId `shouldThrow` isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should create news from an existing draft if the user is an author of it" $ do
      let draftId = DraftId 1
          authorId = AuthorId 2
          draft = draftWithIdAndAuthorId draftId authorId
          user = IdentifiedUser (UserId 0) False [authorId]
          initialData = storageWithDrafts [draft]
          day = ModifiedJulianDay 5
          expectedNews =
            News
              { newsId = createdNewsId
              , newsContent = draftContent draft
              , newsDate = day
              }
      db <- newIORef initialData
      let h = handleWith day db
      r <- run h user draftId
      r `shouldBe` Right expectedNews
      readIORef db `shouldReturn` Storage [] [expectedNews]

data Storage =
  Storage
    { storageDrafts :: [Draft]
    , storageNews :: [News]
    }
  deriving (Eq, Show)

storageWithDrafts :: [Draft] -> Storage
storageWithDrafts drafts = Storage {storageDrafts = drafts, storageNews = []}

handleWith :: Day -> IORef Storage -> Handle IO
handleWith day ref =
  Handle
    { hGetDraftAuthor =
        \searchedId ->
          fmap (fmap authorId . nvAuthor . draftContent) .
          find ((searchedId ==) . draftId) . storageDrafts <$>
          readIORef ref
    , hGetCurrentDay = pure day
    , hMakeDraftIntoNews =
        \searchedDraftId newsDate ->
          updateIORef' ref $ \Storage {..} ->
            let draft =
                  fromJust $ find ((searchedDraftId ==) . draftId) storageDrafts
                news =
                  News
                    { newsId = createdNewsId
                    , newsContent = draftContent draft
                    , newsDate
                    }
             in ( Storage
                    { storageDrafts = delete draft storageDrafts
                    , storageNews = news : storageNews
                    }
                , news)
    }

draftWithId :: DraftId -> Draft
draftWithId draftId = stubDraft {draftId}

draftWithIdAndAuthorId :: DraftId -> AuthorId -> Draft
draftWithIdAndAuthorId draftId authorId =
  stubDraft
    { draftId
    , draftContent = stubNewsVersion {nvAuthor = Existing stubAuthor {authorId}}
    }

draftWithIdAndDeletedAuthor :: DraftId -> Draft
draftWithIdAndDeletedAuthor draftId =
  stubDraft {draftId, draftContent = stubNewsVersion {nvAuthor = Deleted}}

createdNewsId :: NewsId
createdNewsId = NewsId 0
