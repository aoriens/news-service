module Core.Interactor.DeleteCommentSpec
  ( spec
  ) where

import Control.Arrow
import Core.Authentication
import Core.Authentication.Test
import Core.Comment
import Core.Exception
import Core.Interactor.DeleteComment
import Core.News
import Core.User
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should allow an admin to delete an anonymous comment" $ do
      let commentId = CommentId 1
      db <- newIORef $ storageWithItems [anonymousCommentWithId commentId]
      r <- run (handleWith db) someAdminUser commentId
      r `shouldBe` True
      readIORef db `shouldReturn` Storage Map.empty (Set.singleton commentId)
    it "should allow an admin to delete a comment with deleted author" $ do
      let commentId = CommentId 1
      db <-
        newIORef $ storageWithItems [commentWithDeletedAuthorWithId commentId]
      r <- run (handleWith db) someAdminUser commentId
      r `shouldBe` True
      readIORef db `shouldReturn` Storage Map.empty (Set.singleton commentId)
    it
      "should allow an admin to delete a comment posted by another registered user" $ do
      let commentId = CommentId 1
          commentUserId = UserId 1
          authUser =
            IdentifiedUser (UserId $ 1 + getUserId commentUserId) True []
          comment = commentWithIdAndUserId commentId commentUserId
      db <- newIORef $ storageWithItems [comment]
      r <- run (handleWith db) authUser commentId
      r `shouldBe` True
      readIORef db `shouldReturn` Storage Map.empty (Set.singleton commentId)
    it "should allow an admin to delete their own comment" $ do
      let commentId = CommentId 1
          commentUserId = UserId 1
          authUser = IdentifiedUser commentUserId True []
          comment = commentWithIdAndUserId commentId commentUserId
      db <- newIORef $ storageWithItems [comment]
      r <- run (handleWith db) authUser commentId
      r `shouldBe` True
      readIORef db `shouldReturn` Storage Map.empty (Set.singleton commentId)
    it
      "should throw NoPermissionException when a user deletes an anonymous comment" $ do
      let commentId = CommentId 1
          comment = anonymousCommentWithId commentId
          initialData = storageWithItems [comment]
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser commentId `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should throw NoPermissionException when a user deletes a comment with deleted author" $ do
      let commentId = CommentId 1
          comment = commentWithDeletedAuthorWithId commentId
          initialData = storageWithItems [comment]
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser commentId `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should throw NoPermissionException when a user deletes another's comment" $ do
      let commentId = CommentId 1
          commentUserId = UserId 1
          authUser =
            IdentifiedUser (UserId $ 1 + getUserId commentUserId) False []
          comment = commentWithIdAndUserId commentId commentUserId
          initialData = storageWithItems [comment]
      db <- newIORef initialData
      run (handleWith db) authUser commentId `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it "should allow a non-admin user to delete their own comment" $ do
      let commentId = CommentId 1
          commentUserId = UserId 1
          authUser = IdentifiedUser commentUserId False []
          comment = commentWithIdAndUserId commentId commentUserId
      db <- newIORef $ storageWithItems [comment]
      r <- run (handleWith db) authUser commentId
      r `shouldBe` True
      readIORef db `shouldReturn` Storage Map.empty (Set.singleton commentId)
    it
      "should throw NoPermissionException when an anonymous user deletes an anonymous comment" $ do
      let commentId = CommentId 1
          comment = anonymousCommentWithId commentId
          initialData = storageWithItems [comment]
      db <- newIORef initialData
      run (handleWith db) AnonymousUser commentId `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should throw NoPermissionException when a anonymous user deletes some user's comment" $ do
      let commentId = CommentId 1
          comment = commentWithIdAndUserId commentId (UserId 1)
          initialData = storageWithItems [comment]
      db <- newIORef initialData
      run (handleWith db) AnonymousUser commentId `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it "should return False when no comment is found and the user is admin" $ do
      let missingCommentId = CommentId 1
          existingCommentId = CommentId 2
          initialData =
            storageWithItems [stubComment {commentId = existingCommentId}]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser missingCommentId
      r `shouldBe` False
      data' <- readIORef db
      data' `shouldSatisfy`
        (`elem` [ initialData
                , initialData
                    {storageDeletedItems = Set.singleton missingCommentId}
                ])
    it
      "should return False when no comment is found and the user is not an admin" $ do
      let missingCommentId = CommentId 1
          existingCommentId = CommentId 2
          initialData =
            storageWithItems [stubComment {commentId = existingCommentId}]
      db <- newIORef initialData
      r <- run (handleWith db) someNonAdminUser missingCommentId
      r `shouldBe` False
      data' <- readIORef db
      data' `shouldSatisfy`
        (`elem` [ initialData
                , initialData
                    {storageDeletedItems = Set.singleton missingCommentId}
                ])

data Storage =
  Storage
    { storageItems :: Map.HashMap CommentId Comment
    , storageDeletedItems :: Set.HashSet CommentId
    }
  deriving (Eq, Show)

storageWithItems :: [Comment] -> Storage
storageWithItems items =
  Storage
    { storageItems = Map.fromList $ map (commentId &&& id) items
    , storageDeletedItems = Set.empty
    }

handleWith :: IORef Storage -> Handle IO
handleWith db =
  Handle
    { hGetCommentAuthor =
        \commentId ->
          fmap (fmap userId . commentAuthor) .
          Map.lookup commentId . storageItems <$>
          readIORef db
    , hDeleteComment =
        \commentId -> do
          found <- Map.member commentId . storageItems <$> readIORef db
          modifyIORef' db $ \Storage {..} ->
            Storage
              { storageItems = Map.delete commentId storageItems
              , storageDeletedItems = Set.insert commentId storageDeletedItems
              }
          pure found
    }

anonymousCommentWithId :: CommentId -> Comment
anonymousCommentWithId commentId =
  stubComment {commentId, commentAuthor = AnonymousCommentAuthor}

commentWithDeletedAuthorWithId :: CommentId -> Comment
commentWithDeletedAuthorWithId commentId =
  stubComment {commentId, commentAuthor = DeletedCommentAuthor}

commentWithIdAndUserId :: CommentId -> UserId -> Comment
commentWithIdAndUserId commentId userId =
  stubComment {commentId, commentAuthor = UserCommentAuthor stubUser {userId}}

stubComment :: Comment
stubComment =
  Comment
    { commentId = CommentId 0
    , commentNewsId = NewsId 0
    , commentAuthor = AnonymousCommentAuthor
    , commentCreatedAt = UTCTime (ModifiedJulianDay 0) 0
    , commentText = ""
    }

stubUser :: User
stubUser =
  User
    { userId = UserId 0
    , userFirstName = Nothing
    , userLastName = ""
    , userAvatarId = Nothing
    , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
    , userIsAdmin = False
    }
