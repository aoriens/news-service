module Core.Interactor.PublishDraftSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Category
import Core.Deletable
import Core.Exception
import Core.Interactor.PublishDraft
import Core.News
import Core.User
import qualified Data.HashSet as Set
import Data.IORef
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
      let missingDraftId = NewsVersionId 1
          initialData = storageWithDrafts [draftWithId $ NewsVersionId 2]
      db <- newIORef initialData
      let h = handleWith someDay db
      r <- run h someAdminUser missingDraftId
      r `shouldBe` Left UnknownDraftId
      readIORef db `shouldReturn` initialData
    it
      "should throw NoPermissionException if the user is not an author of an existing draft" $ do
      let draftId = NewsVersionId 1
          user = IdentifiedUser (UserId 0) False [AuthorId 2]
          initialData =
            storageWithDrafts [draftWithIdAndAuthorId draftId $ AuthorId 3]
      db <- newIORef initialData
      let h = handleWith someDay db
      run h user draftId `shouldThrow` isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should create news from an existing draft if the user is an author of it" $ do
      let draftId = NewsVersionId 1
          authorId = AuthorId 2
          draft = draftWithIdAndAuthorId draftId authorId
          user = IdentifiedUser (UserId 0) False [authorId]
          initialData = storageWithDrafts [draft]
          day = ModifiedJulianDay 5
          expectedNews =
            News {newsId = createdNewsId, newsVersion = draft, newsDate = day}
      db <- newIORef initialData
      let h = handleWith day db
      r <- run h user draftId
      r `shouldBe` Right expectedNews
      readIORef db `shouldReturn` Storage [] [expectedNews]

data Storage =
  Storage
    { storageDrafts :: [NewsVersion]
    , storageNews :: [News]
    }
  deriving (Eq, Show)

storageWithDrafts :: [NewsVersion] -> Storage
storageWithDrafts drafts = Storage {storageDrafts = drafts, storageNews = []}

handleWith :: Day -> IORef Storage -> Handle IO
handleWith day ref =
  Handle
    { hGetDraftAuthor =
        \searchedId ->
          fmap (authorId . nvAuthor) .
          find ((searchedId ==) . nvId) . storageDrafts <$>
          readIORef ref
    , hGetCurrentDay = pure day
    , hCreateNews =
        \draftId newsDate ->
          updateIORef' ref $ \Storage {..} ->
            let newsVersion =
                  fromJust $ find ((draftId ==) . nvId) storageDrafts
                news = News {newsId = createdNewsId, newsVersion, newsDate}
             in ( Storage
                    { storageDrafts = delete newsVersion storageDrafts
                    , storageNews = news : storageNews
                    }
                , news)
    }

updateIORef' :: IORef a -> (a -> (a, r)) -> IO r
updateIORef' ref f = do
  old <- readIORef ref
  let (new, r) = f old
  writeIORef ref $! new
  pure r

draftWithId :: NewsVersionId -> NewsVersion
draftWithId nvId = stubNewsVersion {nvId}

draftWithIdAndAuthorId :: NewsVersionId -> AuthorId -> NewsVersion
draftWithIdAndAuthorId nvId authorId =
  stubNewsVersion {nvId, nvAuthor = stubAuthor {authorId}}

someDay :: Day
someDay = ModifiedJulianDay 0

createdNewsId :: NewsId
createdNewsId = NewsId 0

stubNewsVersion :: NewsVersion
stubNewsVersion =
  NewsVersion
    { nvId = NewsVersionId 999
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
