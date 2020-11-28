module Core.Interactor.DeleteDraftSpec
  ( spec
  ) where

import Control.Arrow
import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Category
import Core.Exception
import Core.Interactor.DeleteDraft
import Core.News
import Core.Permission
import Core.User
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should throw RequestedEntityNotFoundException if there is no such draft" $ do
      let requestedDraftId = NewsVersionId 1
          existingDraftId = NewsVersionId 2
          initialStorage = newStorage [stubDraft {nvId = existingDraftId}]
      storage <- newIORef initialStorage
      run (handleWith storage) someAuthUser requestedDraftId `shouldThrow`
        isRequestedEntityNotFoundException
      readIORef storage `shouldReturn` initialStorage
    it
      "should throw NoPermissionException if the user is not the author of the draft" $ do
      let draftId = NewsVersionId 1
          draftAuthorId = AuthorId 1
          user = IdentifiedUser (UserId 0) False [AuthorId 2]
          initialStorage =
            newStorage
              [ stubDraft
                  { nvId = draftId
                  , nvAuthor = stubAuthor {authorId = draftAuthorId}
                  }
              ]
      storage <- newIORef initialStorage
      run (handleWith storage) user draftId `shouldThrow`
        isNoPermissionExceptionWithPermission
          (AuthorshipPermission draftAuthorId)
      readIORef storage `shouldReturn` initialStorage
    it "should delete the draft if it is found and the user is the author of it" $ do
      let draftId = NewsVersionId 1
          authorId = AuthorId 1
          user = IdentifiedUser (UserId 0) False [authorId]
          initialStorage =
            newStorage
              [ stubDraft {nvId = draftId, nvAuthor = stubAuthor {authorId}}
              , stubDraft
              ]
      storage <- newIORef initialStorage
      run (handleWith storage) user draftId
      readIORef storage `shouldReturn`
        Storage (itemsMap [stubDraft]) (Set.singleton draftId)

data Storage =
  Storage
    { storageItems :: Map.HashMap NewsVersionId NewsVersion
    , storageDeletedIds :: Set.HashSet NewsVersionId
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
          fmap (authorId . nvAuthor) . Map.lookup draftId . storageItems <$>
          readIORef ref
    , hDeleteNewsVersion =
        \draftId ->
          modifyIORef' ref $ \Storage {..} ->
            Storage
              (Map.delete draftId storageItems)
              (Set.insert draftId storageDeletedIds)
    }

stubDraft :: NewsVersion
stubDraft =
  NewsVersion
    { nvId = NewsVersionId 0
    , nvTitle = "1"
    , nvText = "2"
    , nvAuthor = stubAuthor
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

stubAuthor :: Author
stubAuthor =
  Author
    { authorId = AuthorId 0
    , authorUser =
        User
          { userId = UserId 0
          , userFirstName = Nothing
          , userLastName = ""
          , userAvatarId = Nothing
          , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
          , userIsAdmin = False
          }
    , authorDescription = ""
    }
