{-# LANGUAGE RankNTypes #-}

module Core.Interactor.CreateDraft
  ( run
  , Handle(..)
  , CreateDraftRequest(..)
  , CreateDraftCommand(..)
  , CreateDraftFailure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.Category
import Core.EntityId
import Core.Exception
import Core.Image
import Core.News
import Core.Tag
import Core.User
import qualified Data.HashSet as Set
import Data.Maybe.Util
import qualified Data.Text as T

data Handle m =
  Handle
    { hGetAuthorIdByUserIdIfExactlyOne :: UserId -> m (Maybe AuthorId)
    , hCreateDraft :: CreateDraftCommand -> m (Either CreateDraftFailure Draft)
    , hRejectImageIfDisallowed :: MonadThrow m =>
                                    Image -> m ()
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> CreateDraftRequest
  -> m Draft
run h@Handle {..} authUser request = do
  authorId <- inferAuthorIdFromRequestOrUser h authUser request
  authorize actionName $ authUser `authUserShouldBeAuthor` authorId
  rejectRequestIfInvalid h request
  hCreateDraft (makeCommand request authorId) >>=
    either (throwM . exceptionFromGatewayFailure) pure

inferAuthorIdFromRequestOrUser ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> CreateDraftRequest
  -> m AuthorId
inferAuthorIdFromRequestOrUser _ _ CreateDraftRequest {cdAuthorId = Just authorId'} =
  pure authorId'
inferAuthorIdFromRequestOrUser h authUser _ = do
  userId' <-
    fromMaybeM (throwM AuthenticationRequiredException) $
    authenticatedUserId authUser
  hGetAuthorIdByUserIdIfExactlyOne h userId' >>=
    fromMaybeM (throwM authorAmbiguityException)
  where
    authorAmbiguityException =
      IncorrectParameterException
        "author ID is required: can't determine a unique author ID for the current user"

actionName :: T.Text
actionName = "create a draft"

rejectRequestIfInvalid :: MonadThrow m => Handle m -> CreateDraftRequest -> m ()
rejectRequestIfInvalid Handle {hRejectImageIfDisallowed} CreateDraftRequest {..} = do
  mapM_ (mapM_ hRejectImageIfDisallowed) cdMainPhoto
  mapM_ (mapM_ hRejectImageIfDisallowed) cdAdditionalPhotos

makeCommand :: CreateDraftRequest -> AuthorId -> CreateDraftCommand
makeCommand CreateDraftRequest {..} aid =
  CreateDraftCommand
    { cdcTitle = cdTitle
    , cdcText = cdText
    , cdcAuthorId = aid
    , cdcCategoryId = cdCategoryId
    , cdcMainPhoto = cdMainPhoto
    , cdcAdditionalPhotos = cdAdditionalPhotos
    , cdcTagIds = cdTagIds
    }

exceptionFromGatewayFailure :: CreateDraftFailure -> CoreException
exceptionFromGatewayFailure (CDUnknownEntityId ids) =
  DependentEntitiesNotFoundException ids

data CreateDraftRequest =
  CreateDraftRequest
    { cdTitle :: T.Text
    , cdText :: T.Text
    , cdAuthorId :: Maybe AuthorId
    , cdCategoryId :: Maybe CategoryId
    , cdMainPhoto :: Maybe (Either ImageId Image)
    , cdAdditionalPhotos :: [Either ImageId Image]
    , cdTagIds :: Set.HashSet TagId
    }

data CreateDraftCommand =
  CreateDraftCommand
    { cdcTitle :: T.Text
    , cdcText :: T.Text
    , cdcAuthorId :: AuthorId
    , cdcCategoryId :: Maybe CategoryId
    , cdcMainPhoto :: Maybe (Either ImageId Image)
    , cdcAdditionalPhotos :: [Either ImageId Image]
    , cdcTagIds :: Set.HashSet TagId
    }

newtype CreateDraftFailure =
  CDUnknownEntityId [EntityId]
  deriving (Eq, Show)
