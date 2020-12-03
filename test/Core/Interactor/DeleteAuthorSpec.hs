module Core.Interactor.DeleteAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Interactor.DeleteAuthor
import Core.News
import Core.Stubs
import qualified Data.HashSet as Set
import Data.IORef
import Data.IORef.Util
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should throw NoPermissionException if the user is not an admin" $ do
      let authorId = AuthorId 1
          initialData = newStorage [authorId] [draftWithAuthorId authorId]
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser authorId `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should delete an existing author and return Right () if the user is an admin" $ do
      let existingAuthorId = AuthorId 1
          otherAuthorId = AuthorId 2
          initialData =
            newStorage
              [existingAuthorId, otherAuthorId]
              [ draftWithAuthorId existingAuthorId
              , draftWithAuthorId otherAuthorId
              ]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser existingAuthorId
      r `shouldBe` Right ()
      readIORef db `shouldReturn`
        (newStorage [otherAuthorId] [draftWithAuthorId otherAuthorId])
          { storageRequestedAuthorDeletions = Set.singleton existingAuthorId
          , storageRequestedDraftDeletionsByAuthor =
              Set.singleton existingAuthorId
          }
    it
      "should return Left UnknownAuthorId for an unknown author if the user is an admin" $ do
      let unknownAuthorId = AuthorId 1
          existingAuthorId = AuthorId 2
          initialData =
            newStorage [existingAuthorId] [draftWithAuthorId existingAuthorId]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser unknownAuthorId
      r `shouldBe` Left UnknownAuthorId
      finalData <- readIORef db
      storageAuthors finalData `shouldBe` storageAuthors initialData
      storageDrafts finalData `shouldBe` storageDrafts initialData
      [Set.empty, Set.singleton unknownAuthorId] `shouldContain`
        [storageRequestedAuthorDeletions finalData]
      [Set.empty, Set.singleton unknownAuthorId] `shouldContain`
        [storageRequestedDraftDeletionsByAuthor finalData]

data Storage =
  Storage
    { storageAuthors :: Set.HashSet AuthorId
    , storageDrafts :: [NewsVersion]
    , storageRequestedAuthorDeletions :: Set.HashSet AuthorId
    , storageRequestedDraftDeletionsByAuthor :: Set.HashSet AuthorId
    }
  deriving (Eq, Show)

newStorage :: [AuthorId] -> [NewsVersion] -> Storage
newStorage authorIds drafts =
  Storage
    { storageAuthors = Set.fromList authorIds
    , storageDrafts = drafts
    , storageRequestedAuthorDeletions = Set.empty
    , storageRequestedDraftDeletionsByAuthor = Set.empty
    }

draftWithAuthorId :: AuthorId -> NewsVersion
draftWithAuthorId authorId =
  stubNewsVersion {nvAuthor = Existing stubAuthor {authorId}}

handleWith :: IORef Storage -> Handle IO
handleWith ref =
  Handle
    { hDeleteAuthor =
        \aid ->
          updateIORef' ref $ \old@Storage {..} ->
            ( old
                { storageAuthors = Set.delete aid storageAuthors
                , storageDrafts =
                    map (deleteAuthorWithIdFromDraft aid) storageDrafts
                , storageRequestedAuthorDeletions =
                    Set.insert aid storageRequestedAuthorDeletions
                }
            , if aid `Set.member` storageAuthors
                then Right ()
                else Left UnknownAuthorId)
    , hDeleteDraftsOfAuthor =
        \aid ->
          modifyIORef' ref $ \old@Storage {..} ->
            old
              { storageDrafts =
                  filter
                    ((Existing aid /=) . (fmap authorId . nvAuthor))
                    storageDrafts
              , storageRequestedDraftDeletionsByAuthor =
                  Set.insert aid storageRequestedDraftDeletionsByAuthor
              }
    }

deleteAuthorWithIdFromDraft :: AuthorId -> NewsVersion -> NewsVersion
deleteAuthorWithIdFromDraft targetAuthorId draft
  | Existing targetAuthorId == (authorId <$> nvAuthor draft) =
    draft {nvAuthor = Deleted}
  | otherwise = draft
