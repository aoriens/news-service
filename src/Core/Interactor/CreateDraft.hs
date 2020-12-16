{-# LANGUAGE RankNTypes #-}

module Core.Interactor.CreateDraft
  ( run
  , Handle(..)
  , CreateDraftRequest(..)
  , CreateNewsVersionCommand(..)
  , GatewayFailure(..)
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
import qualified Data.Text as T

data Handle m =
  Handle
    { hAuthorizationHandle :: AuthorizationHandle
    , hGetAuthorIdByUserIdIfExactlyOne :: UserId -> m (Maybe AuthorId)
    , hCreateNewsVersion :: CreateNewsVersionCommand -> m (Either GatewayFailure NewsVersion)
    , hRejectImageIfDisallowed :: MonadThrow m =>
                                    Image -> m ()
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> CreateDraftRequest
  -> m NewsVersion
run h@Handle {..} authUser request = do
  authorId' <- guessAuthorId h authUser request
  requirePermission
    hAuthorizationHandle
    (AuthorshipPermission authorId')
    authUser
    actionName
  rejectRequestIfInvalid h request
  hCreateNewsVersion (makeCommand request authorId') >>=
    either (throwM . exceptionFromGatewayFailure) pure

guessAuthorId ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> CreateDraftRequest
  -> m AuthorId
guessAuthorId _ _ CreateDraftRequest {cdAuthorId = Just authorId'} =
  pure authorId'
guessAuthorId h authUser _ = do
  userId' <-
    maybe (throwM userNotIdentifiedException) pure $
    authenticatedUserId authUser
  hGetAuthorIdByUserIdIfExactlyOne h userId' >>=
    maybe (throwM authorAmbiguityException) pure
  where
    authorAmbiguityException =
      QueryException
        "author ID is required: can't determine a unique author ID for the current user"
    userNotIdentifiedException = UserNotIdentifiedException actionName

actionName :: T.Text
actionName = "create a draft"

rejectRequestIfInvalid :: MonadThrow m => Handle m -> CreateDraftRequest -> m ()
rejectRequestIfInvalid Handle {hRejectImageIfDisallowed} CreateDraftRequest {..} = do
  mapM_ (mapM_ hRejectImageIfDisallowed) cdMainPhoto
  mapM_ (mapM_ hRejectImageIfDisallowed) cdAdditionalPhotos

makeCommand :: CreateDraftRequest -> AuthorId -> CreateNewsVersionCommand
makeCommand CreateDraftRequest {..} aid =
  CreateNewsVersionCommand
    { cnvTitle = cdTitle
    , cnvText = cdText
    , cnvAuthorId = aid
    , cnvCategoryId = cdCategoryId
    , cnvMainPhoto = cdMainPhoto
    , cnvAdditionalPhotos = cdAdditionalPhotos
    , cnvTagIds = cdTagIds
    }

exceptionFromGatewayFailure :: GatewayFailure -> CoreException
exceptionFromGatewayFailure (GUnknownEntityId ids) =
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

data CreateNewsVersionCommand =
  CreateNewsVersionCommand
    { cnvTitle :: T.Text
    , cnvText :: T.Text
    , cnvAuthorId :: AuthorId
    , cnvCategoryId :: Maybe CategoryId
    , cnvMainPhoto :: Maybe (Either ImageId Image)
    , cnvAdditionalPhotos :: [Either ImageId Image]
    , cnvTagIds :: Set.HashSet TagId
    }

newtype GatewayFailure =
  GUnknownEntityId [EntityId]
  deriving (Eq, Show)
