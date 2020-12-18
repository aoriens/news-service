module Core.Interactor.PublishDraftSpec
  ( spec
  ) where

import Control.Arrow
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
      r `shouldBe` Right Success {sNews = expectedNews, sStatus = NewsIsCreated}
      readIORef db `shouldReturn` Storage [] [expectedNews]
    it
      "should update existing news from a draft having been created from the news if the user is an author of the draft" $ do
      let draftId = DraftId 1
          authorId = AuthorId 2
          newsId = NewsId 3
          day = ModifiedJulianDay 4
          oldContent =
            stubNewsVersion
              {nvTitle = "old", nvAuthor = Existing stubAuthor {authorId}}
          newContent =
            stubNewsVersion
              {nvTitle = "new", nvAuthor = Existing stubAuthor {authorId}}
          oldNews = stubNews {newsId, newsContent = oldContent}
          draft =
            stubDraft
              { draftId
              , draftContent = newContent
              , draftNewsIdItWasCreatedFrom = Just newsId
              }
          user = IdentifiedUser (UserId 0) False [authorId]
          initialData = storageWithDraftsAndNews [draft] [oldNews]
          expectedNews = oldNews {newsContent = newContent, newsDate = day}
      db <- newIORef initialData
      let h = handleWith day db
      r <- run h user draftId
      r `shouldBe` Right Success {sNews = expectedNews, sStatus = NewsIsUpdated}
      readIORef db `shouldReturn` Storage [] [expectedNews]

data Storage =
  Storage
    { storageDrafts :: [Draft]
    , storageNews :: [News]
    }
  deriving (Eq, Show)

storageWithDrafts :: [Draft] -> Storage
storageWithDrafts drafts = Storage {storageDrafts = drafts, storageNews = []}

storageWithDraftsAndNews :: [Draft] -> [News] -> Storage
storageWithDraftsAndNews drafts news =
  Storage {storageDrafts = drafts, storageNews = news}

handleWith :: Day -> IORef Storage -> Handle IO
handleWith day ref =
  Handle
    { hGetDraftAuthorAndNewsIdItWasCreatedFrom =
        \searchedId ->
          fmap
            (fmap authorId . nvAuthor . draftContent &&&
             draftNewsIdItWasCreatedFrom) .
          find ((searchedId ==) . draftId) . storageDrafts <$>
          readIORef ref
    , hGetCurrentDay = pure day
    , hMakeDraftIntoNews =
        \searchedDraftId newsDate ->
          updateIORef' ref $ makeDraftIntoNews searchedDraftId newsDate
    , hOverwriteNewsWithDraft =
        \searchedNewsId searchedDraftId newsDate ->
          updateIORef' ref $
          overwriteNewsWithDraft searchedNewsId searchedDraftId newsDate
    }

makeDraftIntoNews ::
     DraftId
  -> Day
  -> Storage
  -> (Storage, Either MakeDraftIntoNewsFailure News)
makeDraftIntoNews searchedDraftId newsDate oldStorage@Storage {..} =
  case find ((searchedDraftId ==) . draftId) storageDrafts of
    Nothing -> (oldStorage, Left MDNUnknownDraftId)
    Just draft ->
      let news =
            News
              { newsId = createdNewsId
              , newsContent = draftContent draft
              , newsDate
              }
       in ( Storage
              { storageDrafts = delete draft storageDrafts
              , storageNews = news : storageNews
              }
          , Right news)

overwriteNewsWithDraft ::
     NewsId
  -> DraftId
  -> Day
  -> Storage
  -> (Storage, Either OverwriteNewsWithDraftFailure News)
overwriteNewsWithDraft searchedNewsId searchedDraftId newsDate oldStorage@Storage {..} =
  case find ((searchedDraftId ==) . draftId) storageDrafts of
    Nothing -> (oldStorage, Left ONDUnknownDraftId)
    Just draft ->
      let oldNews = fromJust $ find ((searchedNewsId ==) . newsId) storageNews
          newNews = oldNews {newsContent = draftContent draft, newsDate}
       in ( Storage
              { storageDrafts = delete draft storageDrafts
              , storageNews = newNews : delete oldNews storageNews
              }
          , Right newNews)

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
