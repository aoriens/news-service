module Core.Interactor.CreateDraftFromNewsSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Interactor.CreateDraftFromNews
import Core.News
import Core.Stubs
import Core.User
import Data.IORef
import Data.IORef.Util
import Data.List
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should return Left UnknownNewsId if news id is unknown" $ do
      let newsId = NewsId 1
      (commandLog, h) <- getHandleWith env
      r <- run h someAuthUser newsId
      r `shouldBe` Left UnknownNewsId
      readIORef commandLog `shouldReturn` []
    it
      "should throw NoPermissionException if the user is not an author of the news" $ do
      let newsId = NewsId 1
          newsAuthorId = AuthorId 1
          unmatchingAuthorId = AuthorId 2
          news =
            stubNews
              { newsId
              , newsContent =
                  stubNewsVersion
                    {nvAuthor = Existing stubAuthor {authorId = newsAuthorId}}
              }
          authUser = IdentifiedUser (UserId 0) True [unmatchingAuthorId]
      (commandLog, h) <- getHandleWith env {envKnownNews = [news]}
      run h authUser newsId `shouldThrow` isNoPermissionException
      readIORef commandLog `shouldReturn` []
    it
      "should throw NoPermissionException if the news is found, but its author is deleted" $ do
      let newsId = NewsId 1
          news =
            stubNews
              {newsId, newsContent = stubNewsVersion {nvAuthor = Deleted}}
          authUser = IdentifiedUser (UserId 0) True [AuthorId 1]
      (commandLog, h) <- getHandleWith env {envKnownNews = [news]}
      run h authUser newsId `shouldThrow` isNoPermissionException
      readIORef commandLog `shouldReturn` []
    it "should create a draft from the news, if the user is an author of it" $ do
      let newsId = NewsId 1
          newsAuthorId = AuthorId 1
          news =
            stubNews
              { newsId
              , newsContent =
                  stubNewsVersion
                    { nvAuthor = Existing stubAuthor {authorId = newsAuthorId}
                    , nvId = NewsVersionId 1
                    }
              }
          authUser = IdentifiedUser (UserId 0) False [newsAuthorId]
          expectedDraft = (newsContent news) {nvId = NewsVersionId 2}
      (commandLog, h) <-
        getHandleWith
          env {envKnownNews = [news], envCopyDraftResult = expectedDraft}
      r <- run h authUser newsId
      r `shouldBe` Right expectedDraft
      readIORef commandLog `shouldReturn` [newsId]

type CopyDraftCommandLog = [NewsId]

data Env =
  Env
    { envKnownNews :: [News]
    , envCopyDraftResult :: NewsVersion
    }

env :: Env
env = Env {envKnownNews = [], envCopyDraftResult = stubNewsVersion}

getHandleWith :: Env -> IO (IORef CopyDraftCommandLog, Handle IO)
getHandleWith Env {..} = do
  commandLog <- newIORef []
  pure
    ( commandLog
    , Handle
        { hGetNewsAuthor =
            \targetId ->
              pure $
              fmap authorId . nvAuthor . newsContent <$>
              find ((targetId ==) . newsId) envKnownNews
        , hCopyDraftFromNews =
            \newsId ->
              updateIORef' commandLog $ \cmdLog ->
                (cmdLog ++ [newsId], envCopyDraftResult)
        })
