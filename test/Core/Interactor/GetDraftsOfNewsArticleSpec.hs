module Core.Interactor.GetDraftsOfNewsArticleSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Interactor.GetDraftsOfNewsArticle
import Core.News
import Core.Pagination
import Core.Stubs
import Core.User
import Data.Functor
import Data.List
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "shoult return Nothing if the newsId is unknown" $ do
      let unknownNewsId = NewsId 1
          h = handleWith env {envKnownNews = []}
      r <- run h someAuthUser unknownNewsId unlimitedPageSpec
      r `shouldBe` Nothing
    it
      "should throw NoPermissionException if the user is not an author of the news" $ do
      let newsId = NewsId 1
          newsAuthorId = AuthorId 1
          mismatchingAuthorId = AuthorId 2
          news = newsWithIdAndAuthor newsId $ Existing newsAuthorId
          user = IdentifiedUser (UserId 0) False [mismatchingAuthorId]
          h = handleWith env {envKnownNews = [news]}
      run h user newsId unlimitedPageSpec `shouldThrow` isNoPermissionException
    it "should throw NoPermissionException if the news author is deleted" $ do
      let newsId = NewsId 1
          news = newsWithIdAndAuthor newsId Deleted
          user = IdentifiedUser (UserId 0) False []
          h = handleWith env {envKnownNews = [news]}
      run h user newsId unlimitedPageSpec `shouldThrow` isNoPermissionException
    it
      "should return drafts created from the news if it exists and the user is its author" $ do
      let newsId = NewsId 1
          otherNewsId = NewsId 2
          newsAuthorId = AuthorId 1
          news = newsWithIdAndAuthor newsId $ Existing newsAuthorId
          user = IdentifiedUser (UserId 0) False [newsAuthorId]
          expectedDrafts =
            [ draftWithTitleAndNewsIdWasItCreatedFrom "1" $ Just newsId
            , draftWithTitleAndNewsIdWasItCreatedFrom "2" $ Just newsId
            ]
          knownDrafts =
            expectedDrafts ++
            [ draftWithTitleAndNewsIdWasItCreatedFrom "3" Nothing
            , draftWithTitleAndNewsIdWasItCreatedFrom "4" $ Just otherNewsId
            ]
          h =
            handleWith env {envKnownNews = [news], envKnownDrafts = knownDrafts}
      r <- run h user newsId unlimitedPageSpec
      r `shouldBe` Just expectedDrafts
    it
      "should follow pageLimit and pageOffset from the page specification, if drafts are to be returned" $ do
      let newsId = NewsId 1
          newsAuthorId = AuthorId 1
          news = newsWithIdAndAuthor newsId $ Existing newsAuthorId
          user = IdentifiedUser (UserId 0) False [newsAuthorId]
          knownDrafts =
            [ draftWithTitleAndNewsIdWasItCreatedFrom "1" $ Just newsId
            , draftWithTitleAndNewsIdWasItCreatedFrom "2" $ Just newsId
            , draftWithTitleAndNewsIdWasItCreatedFrom "3" $ Just newsId
            , draftWithTitleAndNewsIdWasItCreatedFrom "4" $ Just newsId
            ]
          limit = 1
          offset = 2
          pageSpec = PageSpec (PageOffset offset) (PageLimit limit)
          expectedDrafts = genericTake limit $ genericDrop offset knownDrafts
          h =
            handleWith env {envKnownNews = [news], envKnownDrafts = knownDrafts}
      r <- run h user newsId pageSpec
      r `shouldBe` Just expectedDrafts

newsWithIdAndAuthor :: NewsId -> Deletable AuthorId -> News
newsWithIdAndAuthor newsId deletableAuthorId =
  stubNews
    { newsId
    , newsContent =
        stubNewsVersion
          { nvAuthor =
              deletableAuthorId <&> \authorId ->
                stubAuthor {authorId = authorId}
          }
    }

draftWithTitleAndNewsIdWasItCreatedFrom :: T.Text -> Maybe NewsId -> Draft
draftWithTitleAndNewsIdWasItCreatedFrom title newsId =
  stubDraft
    { draftContent = stubNewsVersion {nvTitle = title}
    , draftNewsIdItWasCreatedFrom = newsId
    }

data Env =
  Env
    { envKnownNews :: [News]
    , envKnownDrafts :: [Draft]
    }

env :: Env
env = Env {envKnownNews = [], envKnownDrafts = []}

handleWith :: Env -> Handle IO
handleWith Env {..} =
  Handle
    { hGetNewsAuthorId =
        \searchedNewsId ->
          pure $
          fmap authorId . nvAuthor . newsContent <$>
          find ((searchedNewsId ==) . newsId) envKnownNews
    , hGetDraftsCreatedFromNewsId =
        \searchedNewsId PageSpec {..} ->
          pure .
          genericTake (getPageLimit pageLimit) .
          genericDrop (getPageOffset pageOffset) $
          filter
            ((Just searchedNewsId ==) . draftNewsIdItWasCreatedFrom)
            envKnownDrafts
    }

unlimitedPageSpec :: PageSpec
unlimitedPageSpec = PageSpec (PageOffset 0) (PageLimit maxBound)
