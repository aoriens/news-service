module Core.Interactor.UpdateTag
  ( run
  , Handle(..)
  , Failure(..)
  , SetTagNameFailure(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Tag
import qualified Data.Text as T

data Handle m =
  Handle
    { hFindTagNamed :: T.Text -> m (Maybe TagId)
    , hSetTagName :: TagId -> T.Text -> m (Either SetTagNameFailure ())
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> TagId
  -> T.Text
  -> m (Either Failure Tag)
run Handle {..} authUser tagId newTagName = do
  authorize "modify a tag" $ authUserShouldBeAdmin authUser
  if T.null newTagName
    then pure $ Left TagNameMustNotBeEmpty
    else goAssumingValidName
  where
    goAssumingValidName =
      hFindTagNamed newTagName >>= \case
        Just existingTagId
          | tagId == existingTagId -> pure $ Right updatedTag
          | otherwise -> pure $ Left TagNameMustBeUnique
        Nothing ->
          hSetTagName tagId newTagName >>= \case
            Left STNUnknownTagId -> pure $ Left UnknownTagId
            Right () -> pure $ Right updatedTag
    updatedTag = Tag {tagId, tagName = newTagName}

data Failure
  = UnknownTagId
  | TagNameMustNotBeEmpty
  | TagNameMustBeUnique
  deriving (Eq, Show)

data SetTagNameFailure =
  STNUnknownTagId
