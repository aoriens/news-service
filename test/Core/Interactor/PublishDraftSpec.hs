module Core.Interactor.PublishDraftSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
import Core.Category
import Core.Exception
import Core.Interactor.PublishDraft
import Core.News
import Core.User
import qualified Data.HashSet as Set
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    let documentAuthorId = AuthorId 6
    itShouldAuthorizeBeforeOperation (AuthorshipPermission documentAuthorId) $ \authUser authorizationHandle onSuccess -> do
      let h =
            stubHandle
              { hAuthorizationHandle = authorizationHandle
              , hGetDraftAuthor = \_ -> pure $ Right documentAuthorId
              , hCreateNews = \_ _ -> onSuccess >> pure stubNews
              }
          draftId = NewsVersionId 1
      _ <- run h authUser draftId
      pure ()
    it
      "should throw RequestedEntityNotFoundException if hGetDraftAuthor returns Left UnknownNewsVersionId" $ do
      let h =
            stubHandle
              { hGetDraftAuthor = \_ -> pure $ Left UnknownDraftId
              , hCreateNews = \_ _ -> error "Must not invoke"
              }
          draftId = NewsVersionId 1
      run h anyAuthenticatedUser draftId `shouldThrow`
        isRequestedEntityNotFoundException
    it "should pass authorId to authorization from hGetDraftAuthor" $ do
      let authorId' = AuthorId 1
          h =
            stubHandle
              { hGetDraftAuthor = \_ -> pure $ Right authorId'
              , hAuthorizationHandle =
                  AuthorizationHandle $ \perm _ ->
                    perm == AuthorshipPermission authorId'
              }
          draftId = NewsVersionId 1
      _ <- run h anyAuthenticatedUser draftId
      pure ()
    it "should pass draftId to hGetDraftAuthor" $ do
      passedDraftIds <- newIORef []
      let h =
            stubHandle
              { hGetDraftAuthor =
                  \id' -> do
                    modifyIORef' passedDraftIds (id' :)
                    pure . Right $ AuthorId 1
              }
          draftId = NewsVersionId 1
      _ <- run h anyAuthenticatedUser draftId
      readIORef passedDraftIds `shouldReturn` [draftId]
    it "should pass date from hGetCurrentDay to hCreateNews" $ do
      passedDates <- newIORef []
      let expectedDay = ModifiedJulianDay 6
          h =
            stubHandle
              { hGetCurrentDay = pure expectedDay
              , hCreateNews =
                  \_ day -> modifyIORef' passedDates (day :) >> pure stubNews
              }
          draftId = NewsVersionId 1
      _ <- run h anyAuthenticatedUser draftId
      readIORef passedDates `shouldReturn` [expectedDay]
    it "should pass draftId to hCreateNews" $ do
      passedIds <- newIORef []
      let h =
            stubHandle
              { hCreateNews =
                  \id' _ -> modifyIORef' passedIds (id' :) >> pure stubNews
              }
          draftId = NewsVersionId 1
      _ <- run h anyAuthenticatedUser draftId
      readIORef passedIds `shouldReturn` [draftId]
    it "should return news from hCreateNews" $ do
      let expectedNews = stubNews {newsId = NewsId 1}
          h = stubHandle {hCreateNews = \_ _ -> pure expectedNews}
          draftId = NewsVersionId 1
      news <- run h anyAuthenticatedUser draftId
      news `shouldBe` expectedNews

stubNews :: News
stubNews =
  News
    { newsId = NewsId 999
    , newsDate = ModifiedJulianDay 0
    , newsVersion =
        NewsVersion
          { nvId = NewsVersionId 999
          , nvTitle = "1"
          , nvText = "2"
          , nvAuthor =
              Author
                { authorId = AuthorId 999
                , authorUser =
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
    }

stubHandle :: Handle IO
stubHandle =
  Handle
    { hAuthorizationHandle = noOpAuthorizationHandle
    , hGetDraftAuthor = \_ -> pure $ Right $ AuthorId 999
    , hGetCurrentDay = pure $ ModifiedJulianDay 0
    , hCreateNews = \_ _ -> pure stubNews
    }
