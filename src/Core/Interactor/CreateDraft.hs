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
import qualified Data.HashSet as Set
import qualified Data.Text as T

data Handle m =
  Handle
    { hAuthenticationHandle :: AuthenticationHandle m
    , hAuthorizationHandle :: AuthorizationHandle
    , hCreateNewsVersion :: CreateNewsVersionCommand -> m (Either GatewayFailure NewsVersion)
    }

run ::
     MonadThrow m
  => Handle m
  -> Maybe Credentials
  -> CreateDraftRequest
  -> m NewsVersion
run Handle {..} credentials request = do
  actor <- authenticate hAuthenticationHandle credentials
  requireAuthorshipPermission
    hAuthorizationHandle
    (cdAuthorId request)
    actor
    "create a draft"
  hCreateNewsVersion (commandFromRequest request) >>=
    either (throwM . exceptionFromGatewayFailure) pure

commandFromRequest :: CreateDraftRequest -> CreateNewsVersionCommand
commandFromRequest CreateDraftRequest {..} =
  CreateNewsVersionCommand
    { cnvTitle = cdTitle
    , cnvText = cdText
    , cnvAuthorId = cdAuthorId
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
    , cdAuthorId :: AuthorId
    , cdCategoryId :: CategoryId
    , cdMainPhoto :: Maybe (Either ImageId Image)
    , cdAdditionalPhotos :: [Either ImageId Image]
    , cdTagIds :: Set.HashSet TagId
    }

data CreateNewsVersionCommand =
  CreateNewsVersionCommand
    { cnvTitle :: T.Text
    , cnvText :: T.Text
    , cnvAuthorId :: AuthorId
    , cnvCategoryId :: CategoryId
    , cnvMainPhoto :: Maybe (Either ImageId Image)
    , cnvAdditionalPhotos :: [Either ImageId Image]
    , cnvTagIds :: Set.HashSet TagId
    }

newtype GatewayFailure =
  GUnknownEntityId [EntityId]
  deriving (Eq, Show)
