module Core.Interactor.DeleteTagSpec
  ( spec
  ) where

import Core.Authentication.Test
import Core.Authorization
import Core.Exception
import Core.Interactor.DeleteTag
import Core.Tag
import qualified Data.HashSet as Set
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should throw NoPermissionException if the user is not an admin" $ do
      let existingTagId = TagId 1
          initialData = storageWithItems [existingTagId]
      db <- newIORef initialData
      run (handleWith db) someNonAdminUser existingTagId `shouldThrow`
        isNoPermissionExceptionWithPermission AdminPermission
      readIORef db `shouldReturn` initialData
    it "should remove an existing tag and return True" $ do
      let existingTagId = TagId 1
          initialData = storageWithItems [existingTagId]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser existingTagId
      r `shouldBe` True
      readIORef db `shouldReturn`
        Storage Set.empty (Set.singleton existingTagId)
    it "should not remove a non-existing tag and return False" $ do
      let existingTagId = TagId 1
          missingTagId = TagId 2
          initialData = storageWithItems [existingTagId]
      db <- newIORef initialData
      r <- run (handleWith db) someAdminUser missingTagId
      r `shouldBe` False
      readIORef db `shouldReturn`
        initialData {storageDeletedItems = Set.singleton missingTagId}

data Storage =
  Storage
    { storageItems :: Set.HashSet TagId
    , storageDeletedItems :: Set.HashSet TagId
    }
  deriving (Eq, Show)

storageWithItems :: [TagId] -> Storage
storageWithItems items =
  Storage {storageItems = Set.fromList items, storageDeletedItems = Set.empty}

handleWith :: IORef Storage -> Handle IO
handleWith db =
  Handle
    { hDeleteTag =
        \tagId -> do
          found <- Set.member tagId . storageItems <$> readIORef db
          modifyIORef' db $ \Storage {..} ->
            Storage
              { storageItems = Set.delete tagId storageItems
              , storageDeletedItems = Set.insert tagId storageDeletedItems
              }
          pure found
    }
