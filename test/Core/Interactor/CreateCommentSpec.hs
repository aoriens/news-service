module Core.Interactor.CreateCommentSpec
  ( spec
  ) where

import Control.Exception
import Core.Authentication
import Core.Authentication.Test
import Core.Comment
import Core.EntityId
import Core.Exception
import Core.Interactor.CreateComment
import Core.News
import Core.Stubs
import Core.User
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should return result of hCreateComment if it is wrapped with Right" $ do
      let expectedComment = stubComment {commentText = "q"}
          h =
            stubHandle
              {hCreateComment = \_ _ _ _ -> pure $ Right expectedComment}
      r <- run h someAuthUser "" (NewsId 0)
      r `shouldBe` expectedComment
    it
      "should throw DependentEntitiesNotFoundException with failing entities ID if hCreateComment returns Left GUnknownEntityId" $ do
      let expectedEntityId = toEntityId $ NewsId 1
          h =
            stubHandle
              { hCreateComment =
                  \_ _ _ _ -> pure $ Left $ GUnknownEntityId expectedEntityId
              }
      r <- try $ run h someAuthUser "" (NewsId 0)
      r `shouldBe` Left (DependentEntitiesNotFoundException [expectedEntityId])
    it "should pass comment text to hCreateComment" $ do
      let expectedText = "1"
          h =
            stubHandle
              { hCreateComment =
                  \text _ _ _ -> pure $ Right stubComment {commentText = text}
              }
      r <- run h someAuthUser expectedText (NewsId 0)
      commentText r `shouldBe` expectedText
    it "should pass newsId to hCreateComment" $ do
      let expectedNewsId = NewsId 1
          h =
            stubHandle
              { hCreateComment =
                  \_ _ newsId' _ ->
                    pure $ Right stubComment {commentNewsId = newsId'}
              }
      r <- run h someAuthUser "" expectedNewsId
      commentNewsId r `shouldBe` expectedNewsId
    it "should pass result of hGetCurrentTime to hCreateComment" $ do
      let expectedTime = UTCTime (ModifiedJulianDay 1) 1
          h =
            stubHandle
              { hCreateComment =
                  \_ _ _ time ->
                    pure $ Right stubComment {commentCreatedAt = time}
              , hGetCurrentTime = pure expectedTime
              }
      r <- run h someAuthUser "" (NewsId 0)
      commentCreatedAt r `shouldBe` expectedTime
    it
      "should pass Nothing as userId to hCreateComment if the auth user is anonymous" $ do
      passedIds <- newIORef []
      let h =
            stubHandle
              { hCreateComment =
                  \_ userId' _ _ -> do
                    modifyIORef' passedIds (userId' :)
                    pure $ Right stubComment
              }
      _ <- run h AnonymousUser "" (NewsId 0)
      readIORef passedIds `shouldReturn` [Nothing]
    it
      "should pass UserId from IdentifiedUse to hCreateComment if the auth user is identified" $ do
      passedIds <- newIORef []
      let expectedUserId = UserId 1
          authUser = IdentifiedUser expectedUserId False []
          h =
            stubHandle
              { hCreateComment =
                  \_ userId' _ _ -> do
                    modifyIORef' passedIds (userId' :)
                    pure $ Right stubComment
              }
      _ <- run h authUser "" (NewsId 0)
      readIORef passedIds `shouldReturn` [Just expectedUserId]

stubTime :: UTCTime
stubTime = UTCTime (ModifiedJulianDay 0) 0

stubHandle :: Handle IO
stubHandle =
  Handle
    { hCreateComment = \_ _ _ _ -> pure $ Right stubComment
    , hGetCurrentTime = pure stubTime
    }
