module Core.Interactor.DeleteAuthorSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Author
import Core.Exception
import Core.Interactor.DeleteAuthor
import qualified Data.HashSet as Set
import Data.IORef
import Data.IORef.Util
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should throw NoPermissionException if the user is not an admin" $ do
      let authorId = AuthorId 1
          initialData = storageWithAuthors [authorId]
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser authorId `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it
      "should delete an existing author and return True if the user is an admin" $ do
      let existingAuthorId = AuthorId 1
          otherAuthorId = AuthorId 2
          initialData = storageWithAuthors [existingAuthorId, otherAuthorId]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser existingAuthorId
      r `shouldBe` True
      readIORef db `shouldReturn`
        Storage (Set.singleton otherAuthorId) (Set.singleton existingAuthorId)
    it
      "should not delete an unknown author and return False if the user is an admin" $ do
      let unknownAuthorId = AuthorId 1
          initialData = storageWithAuthors [AuthorId 2]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser unknownAuthorId
      r `shouldBe` False
      readIORef db `shouldReturn`
        initialData {storageRequestedDeletions = Set.singleton unknownAuthorId}

data Storage =
  Storage
    { storageAuthors :: Set.HashSet AuthorId
    , storageRequestedDeletions :: Set.HashSet AuthorId
    }
  deriving (Eq, Show)

storageWithAuthors :: [AuthorId] -> Storage
storageWithAuthors ids = Storage (Set.fromList ids) Set.empty

handleWith :: IORef Storage -> Handle IO
handleWith ref =
  Handle $ \authorId ->
    updateIORef' ref $ \Storage {..} ->
      ( Storage
          { storageAuthors = Set.delete authorId storageAuthors
          , storageRequestedDeletions =
              Set.insert authorId storageRequestedDeletions
          }
      , authorId `Set.member` storageAuthors)
