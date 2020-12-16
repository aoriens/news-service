module Core.Interactor.DeleteDraftSpec
  ( spec
  ) where

import Control.Arrow
import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Interactor.DeleteDraft
import Core.News
import Core.Permission
import Core.Stubs
import Core.User
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should return Left UnknownDraftId if there is no such draft" $ do
      let requestedDraftId = NewsVersionId 1
          existingDraftId = NewsVersionId 2
          initialStorage = newStorage [stubNewsVersion {nvId = existingDraftId}]
      storage <- newIORef initialStorage
      r <- run (handleWith storage) someAuthUser requestedDraftId
      r `shouldBe` Left UnknownDraftId
      readIORef storage `shouldReturn` initialStorage
    it
      "should return Left UnknownDraftId for an existing draft with the author deleted" $ do
      let draftId = NewsVersionId 1
          initialStorage =
            newStorage [stubNewsVersion {nvId = draftId, nvAuthor = Deleted}]
      storage <- newIORef initialStorage
      r <- run (handleWith storage) someAuthUser draftId
      r `shouldBe` Left UnknownDraftId
      readIORef storage `shouldReturn` initialStorage
    it
      "should throw NoPermissionException if the user is not the author of the draft" $ do
      let draftId = NewsVersionId 1
          draftAuthorId = AuthorId 1
          user = IdentifiedUser (UserId 0) False [AuthorId 2]
          initialStorage =
            newStorage
              [ stubNewsVersion
                  { nvId = draftId
                  , nvAuthor = Existing stubAuthor {authorId = draftAuthorId}
                  }
              ]
      storage <- newIORef initialStorage
      run (handleWith storage) user draftId `shouldThrow`
        isNoPermissionExceptionWithPermission
          (AuthorshipPermission $ Existing draftAuthorId)
      readIORef storage `shouldReturn` initialStorage
    it "should delete the draft if it is found and the user is the author of it" $ do
      let draftId = NewsVersionId 1
          authorId = AuthorId 1
          user = IdentifiedUser (UserId 0) False [authorId]
          initialStorage =
            newStorage
              [ stubNewsVersion
                  {nvId = draftId, nvAuthor = Existing stubAuthor {authorId}}
              , stubNewsVersion
              ]
      storage <- newIORef initialStorage
      r <- run (handleWith storage) user draftId
      r `shouldBe` Right ()
      readIORef storage `shouldReturn`
        Storage (itemsMap [stubNewsVersion]) (Set.singleton draftId)

data Storage =
  Storage
    { storageDrafts :: Map.HashMap NewsVersionId NewsVersion
    , storageRequestedDeletions :: Set.HashSet NewsVersionId
    }
  deriving (Eq, Show)

newStorage :: [NewsVersion] -> Storage
newStorage items = Storage (itemsMap items) Set.empty

itemsMap :: [NewsVersion] -> Map.HashMap NewsVersionId NewsVersion
itemsMap = Map.fromList . map (nvId &&& id)

handleWith :: IORef Storage -> Handle IO
handleWith ref =
  Handle
    { hGetDraftAuthor =
        \draftId ->
          fmap (fmap authorId . nvAuthor) . Map.lookup draftId . storageDrafts <$>
          readIORef ref
    , hDeleteNewsVersion =
        \draftId ->
          modifyIORef' ref $ \Storage {..} ->
            Storage
              (Map.delete draftId storageDrafts)
              (Set.insert draftId storageRequestedDeletions)
    }
