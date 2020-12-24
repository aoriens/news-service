module Core.Interactor.UpdateDraft
  ( run
  , Handle(..)
  , UpdateDraftRequest(..)
  , Failure(..)
  , UpdateDraftFailure(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Authorization
import Core.Category
import Core.Deletable
import Core.EntityId
import Core.Image
import Core.News
import Core.Tag
import Data.Bifunctor
import qualified Data.HashSet as Set
import qualified Data.Text as T

data Handle m =
  Handle
    { hGetDraftAuthor :: DraftId -> m (Maybe (Deletable AuthorId))
    , hUpdateDraft :: DraftId -> UpdateDraftRequest -> m (Either UpdateDraftFailure Draft)
    }

-- | The request to update a draft. 'Nothing' value of a field means
-- leaving unmodified, and 'Just x' means overwriting the field with
-- value @x@.
data UpdateDraftRequest =
  UpdateDraftRequest
    { udrTitle :: Maybe T.Text
    , udrText :: Maybe T.Text
    , udrMainPhoto :: Maybe (Maybe (Either ImageId Image))
    , udrAdditionalPhotos :: Maybe [Either ImageId Image]
    , udrCategory :: Maybe (Maybe CategoryId)
    , udrTags :: Maybe (Set.HashSet TagId)
    }
  deriving (Eq, Show)

newtype UpdateDraftFailure =
  UDUnknownEntityIds [EntityId]
  deriving (Eq, Show)

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> DraftId
  -> UpdateDraftRequest
  -> m (Either Failure Draft)
run Handle {..} authUser draftId request = do
  hGetDraftAuthor draftId >>= \case
    Nothing -> pure . Left $ UnknownEntityId [toEntityId draftId]
    Just authorId -> do
      authorize "update a draft" $
        authUser `authUserShouldBeDeletableAuthor` authorId
      first fromUpdateDraftFailure <$> hUpdateDraft draftId request

newtype Failure =
  UnknownEntityId [EntityId]
  deriving (Eq, Show)

fromUpdateDraftFailure :: UpdateDraftFailure -> Failure
fromUpdateDraftFailure (UDUnknownEntityIds ids) = UnknownEntityId ids
