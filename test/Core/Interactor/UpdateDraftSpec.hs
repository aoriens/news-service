module Core.Interactor.UpdateDraftSpec
  ( spec
  ) where

import Core.Authentication
import Core.Authentication.Test
import Core.Author
import Core.Deletable
import Core.Exception
import Core.Interactor.UpdateDraft
import Core.News
import Core.Stubs
import Core.User
import Data.Functor
import Data.IORef
import Data.List
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    it "should return Left UnknownEntityId _ if the draft is not found" $ do
      let unknownDraftId = DraftId 1
      commandLog <- newIORef []
      let h = handleWith env commandLog
      r <- run h someAuthUser unknownDraftId stubRequest
      r `shouldSatisfy` isUnknownEntityIdResult
      readIORef commandLog `shouldReturn` []
    it
      "should throw NoPermissionException if the user is not the author of the draft" $ do
      let draftId = DraftId 1
          draftAuthorId = AuthorId 2
          otherAuthorId = AuthorId 3
          draft = makeDraft draftId $ Existing draftAuthorId
          authUser = IdentifiedUser (UserId 4) False [otherAuthorId]
      commandLog <- newIORef []
      let h = handleWith env {envKnownDrafts = [draft]} commandLog
      run h authUser draftId stubRequest `shouldThrow` isNoPermissionException
      readIORef commandLog `shouldReturn` []
    it
      "should update the draft and return the updated one if it's found and the user is its author" $ do
      let draftId = DraftId 1
          draftAuthorId = AuthorId 2
          draft = makeDraft draftId $ Existing draftAuthorId
          authUser = IdentifiedUser (UserId 4) False [draftAuthorId]
          newTitle = "new title"
          request = stubRequest {udrTitle = Just newTitle}
          expectedDraft =
            draft {draftContent = (draftContent draft) {nvTitle = newTitle}}
      commandLog <- newIORef []
      let h =
            handleWith
              env {envKnownDrafts = [draft], envExpectedResult = expectedDraft}
              commandLog
      r <- run h authUser draftId request
      r `shouldBe` Right expectedDraft
      readIORef commandLog `shouldReturn` [(draftId, request)]

stubRequest :: UpdateDraftRequest
stubRequest =
  UpdateDraftRequest
    { udrTitle = Nothing
    , udrText = Nothing
    , udrCategory = Nothing
    , udrMainPhoto = Nothing
    , udrAdditionalPhotos = Nothing
    , udrTags = Nothing
    }

makeDraft :: DraftId -> Deletable AuthorId -> Draft
makeDraft draftId authorId =
  stubDraft
    { draftId
    , draftContent =
        stubNewsVersion
          {nvAuthor = authorId <&> \id' -> stubAuthor {authorId = id'}}
    }

data Env =
  Env
    { envKnownDrafts :: [Draft]
    , envExpectedResult :: Draft
    }

env :: Env
env = Env {envKnownDrafts = [], envExpectedResult = stubDraft}

type UpdateCommandLog = [(DraftId, UpdateDraftRequest)]

handleWith :: Env -> IORef UpdateCommandLog -> Handle IO
handleWith Env {..} logRef =
  Handle
    { hGetDraftAuthor =
        \searchedDraftId ->
          pure $
          fmap authorId . nvAuthor . draftContent <$>
          find ((searchedDraftId ==) . draftId) envKnownDrafts
    , hUpdateDraft =
        \draftId request -> do
          modifyIORef' logRef (++ [(draftId, request)])
          pure $ Right envExpectedResult
    }

isUnknownEntityIdResult :: Either Failure a -> Bool
isUnknownEntityIdResult =
  \case
    Left (UnknownEntityId _) -> True
    _ -> False
