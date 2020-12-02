module Core.Interactor.DeleteUserSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Test
import Core.Exception
import Core.Interactor.DeleteUser
import Core.User
import qualified Data.HashSet as Set
import Data.IORef
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should throw NoPermissionException if the user is not an admin" $ do
      let userIdToDelete = UserId 1
          initialData = storageWithItems [userIdToDelete]
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser userIdToDelete `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it "should throw NoPermissionException if the user is anonymous" $ do
      let userIdToDelete = UserId 1
          initialData = storageWithItems [userIdToDelete]
      db <- newIORef initialData
      run (handleWith db) AnonymousUser userIdToDelete `shouldThrow`
        isNoPermissionException
      readIORef db `shouldReturn` initialData
    it "should delete an existing user and return True if the actor is an admin" $ do
      let userIdToDelete = UserId 1
          initialData = storageWithItems [userIdToDelete]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser userIdToDelete
      r `shouldBe` True
      readIORef db `shouldReturn`
        Storage Set.empty (Set.singleton userIdToDelete)
    it
      "should not delete an unknown user and return False if the actor is an admin" $ do
      let userIdToDelete = UserId 1
          initialData = storageWithItems [UserId 2]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser userIdToDelete
      r `shouldBe` False
      readIORef db `shouldReturn`
        initialData {storageRequestedDeletions = Set.singleton userIdToDelete}

data Storage =
  Storage
    { storageItems :: Set.HashSet UserId
    , storageRequestedDeletions :: Set.HashSet UserId
    }
  deriving (Eq, Show)

storageWithItems :: [UserId] -> Storage
storageWithItems items =
  Storage
    {storageItems = Set.fromList items, storageRequestedDeletions = Set.empty}

handleWith :: IORef Storage -> Handle IO
handleWith db =
  Handle
    { hDeleteUser =
        \userId -> do
          found <- Set.member userId . storageItems <$> readIORef db
          modifyIORef' db $ \Storage {..} ->
            Storage
              { storageItems = Set.delete userId storageItems
              , storageRequestedDeletions =
                  Set.insert userId storageRequestedDeletions
              }
          pure found
    }
