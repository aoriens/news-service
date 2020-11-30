module Core.Interactor.GetCommentsForNewsSpec
  ( spec
  ) where

import Control.Exception
import Control.Monad
import Core.Comment
import Core.EntityId
import Core.Exception
import Core.Interactor.GetCommentsForNews
import Core.News
import Core.Pagination
import Core.Pagination.Test
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            defaultHandle
              { hGetCommentsForNews =
                  \_ pageQuery -> onSuccess pageQuery >> pure (Right [])
              , hPageSpecParserHandle
              }
      void $ run h (NewsId 0) pageSpecQuery
    it "should return comments from hGetCommentsFromNews if it returned Right _" $ do
      let expectedComments = [stubComment {commentText = "q"}]
          h =
            defaultHandle
              {hGetCommentsForNews = \_ _ -> pure $ Right expectedComments}
      r <- run h (NewsId 0) noPageQuery
      r `shouldBe` expectedComments
    it
      "should throw RequestedEntityNotFoundException if hGetCommentsForNews returned Left _" $ do
      let newsId = NewsId 0
          h =
            defaultHandle
              {hGetCommentsForNews = \_ _ -> pure $ Left GUnknownNewsId}
      r <- try $ run h newsId noPageQuery
      r `shouldBe` Left (RequestedEntityNotFoundException (toEntityId newsId))

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetCommentsForNews = \_ _ -> pure $ Right []
    , hPageSpecParserHandle = PageSpecParserHandle . const $ Right defaultPage
    }

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

defaultPage :: PageSpec
defaultPage = PageSpec (PageOffset 0) (PageLimit 0)

stubComment :: Comment
stubComment =
  Comment
    { commentId = CommentId 0
    , commentNewsId = NewsId 0
    , commentAuthor = AnonymousCommentAuthor
    , commentCreatedAt = UTCTime (ModifiedJulianDay 0) 0
    , commentText = ""
    }
