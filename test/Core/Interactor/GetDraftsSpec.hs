module Core.Interactor.GetDraftsSpec
  ( spec
  ) where

import Control.Exception
import Control.Monad
import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Exception
import Core.Interactor.GetDrafts
import Core.News
import Core.Pagination
import Core.Pagination.Test
import Core.Stubs
import Core.User
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it
      "should throw AuthenticationRequiredException if the user is not authenticated" $ do
      let h = defaultHandle
      r <- try $ run h AnonymousUser Nothing noPageQuery
      r `shouldBe` Left AuthenticationRequiredException
    describe "when no authorId is passed" $ do
      itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
        let h =
              defaultHandle
                { hPageSpecParserHandle
                , hGetDraftsOfUser =
                    \_ pageSpec -> onSuccess pageSpec >> pure []
                }
        void $ run h someIdentifiedAuthUser Nothing pageSpecQuery
    describe "when an authorId is passed" $ do
      itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
        let h =
              defaultHandle
                { hPageSpecParserHandle
                , hGetDraftsOfAuthor =
                    \_ pageSpec -> onSuccess pageSpec >> pure []
                }
            authUser = IdentifiedUser (UserId 0) False [authorId]
            authorId = AuthorId 1
        void $ run h authUser (Just authorId) pageSpecQuery
    it
      "should invoke hGetDraftsOfUser once and should not invoke hGetDraftsOfAuthor if no author is passed" $ do
      ref <- newIORef []
      let author = Nothing
          h =
            defaultHandle
              { hGetDraftsOfUser =
                  \_ _ -> modifyIORef ref ("by user" :) >> pure []
              , hGetDraftsOfAuthor =
                  \_ _ -> modifyIORef ref ("by author" :) >> pure []
              }
      _ <- run h someIdentifiedAuthUser author noPageQuery
      readIORef ref `shouldReturn` ["by user" :: String]
    it
      "should pass authenticated user id to hGetDraftsOfUser if no author is passed" $ do
      ref <- newIORef Nothing
      let expectedUserId = UserId 1
          authUser = IdentifiedUser expectedUserId False []
          author = Nothing
          h =
            defaultHandle
              { hGetDraftsOfUser =
                  \userId _ -> writeIORef ref (Just userId) >> pure []
              }
      _ <- run h authUser author noPageQuery
      readIORef ref `shouldReturn` Just expectedUserId
    it "should pass pageSpec to hGetDraftsOfUser if no author is passed" $ do
      ref <- newIORef Nothing
      let expectedPageSpec = PageSpec (PageOffset 1) (PageLimit 2)
          author = Nothing
          h =
            defaultHandle
              { hGetDraftsOfUser =
                  \_ pageSpec -> writeIORef ref (Just pageSpec) >> pure []
              , hPageSpecParserHandle =
                  PageSpecParserHandle $ const $ Right expectedPageSpec
              }
      _ <- run h someIdentifiedAuthUser author noPageQuery
      readIORef ref `shouldReturn` Just expectedPageSpec
    it "should return result of hGetDraftsOfUser if no author is passed" $ do
      let expectedDrafts = [stubDraft {draftId = DraftId 1}]
          author = Nothing
          h = defaultHandle {hGetDraftsOfUser = \_ _ -> pure expectedDrafts}
      r <- run h someIdentifiedAuthUser author noPageQuery
      r `shouldBe` expectedDrafts
    it "should pass authorId to hGetDraftsOfUser if not Nothing" $ do
      ref <- newIORef Nothing
      let expectedAuthorId = AuthorId 1
          authUser = IdentifiedUser (UserId 0) False [expectedAuthorId]
          h =
            defaultHandle
              { hGetDraftsOfAuthor =
                  \authorId _ -> writeIORef ref (Just authorId) >> pure []
              }
      _ <- run h authUser (Just expectedAuthorId) noPageQuery
      readIORef ref `shouldReturn` Just expectedAuthorId
    it "should pass pageSpec to hGetDraftsOfAuthor if an author is passed" $ do
      ref <- newIORef Nothing
      let expectedPageSpec = PageSpec (PageOffset 1) (PageLimit 2)
          authorId = AuthorId 1
          authUser = IdentifiedUser (UserId 0) False [authorId]
          h =
            defaultHandle
              { hGetDraftsOfAuthor =
                  \_ pageSpec -> writeIORef ref (Just pageSpec) >> pure []
              , hPageSpecParserHandle =
                  PageSpecParserHandle $ const $ Right expectedPageSpec
              }
      _ <- run h authUser (Just authorId) noPageQuery
      readIORef ref `shouldReturn` Just expectedPageSpec
    it "should return result of hGetDraftsOfAuthor if an author is passed" $ do
      let expectedDrafts = [stubDraft {draftId = DraftId 1}]
          authorId = AuthorId 1
          authUser = IdentifiedUser (UserId 0) False [authorId]
          h = defaultHandle {hGetDraftsOfAuthor = \_ _ -> pure expectedDrafts}
      r <- run h authUser (Just authorId) noPageQuery
      r `shouldBe` expectedDrafts
    it
      "should throw NoPermissionException if the authorId is passed which does not relate to the user" $ do
      let ownedAuthorId = AuthorId 1
          unownedAuthorId = AuthorId 2
          authUser = IdentifiedUser (UserId 0) False [ownedAuthorId]
          h = defaultHandle
      run h authUser (Just unownedAuthorId) noPageQuery `shouldThrow`
        isNoPermissionException

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetDraftsOfUser = \_ _ -> pure []
    , hGetDraftsOfAuthor = \_ _ -> pure []
    , hPageSpecParserHandle = PageSpecParserHandle . const $ Right defaultPage
    }

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

defaultPage :: PageSpec
defaultPage = PageSpec (PageOffset 0) (PageLimit 0)
