module Core.Interactor.CreateCategory
  ( run
  , Handle(..)
  , Failure(..)
  , CreateCategoryFailure(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Category
import Data.Bifunctor
import Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T

data Handle m =
  Handle
    { hCreateCategory :: Maybe CategoryId -> NonEmpty T.Text -> m (Either CreateCategoryFailure Category)
    , hCategoryIdWithParentAndNameExists :: Maybe CategoryId -> T.Text -> m Bool
    }

data CreateCategoryFailure =
  CCFUnknownParentCategoryId
  deriving (Show, Eq)

data Failure
  = UnknownParentCategoryId
  | CategoryNameMustNotBeEmpty
  | CategoryNameMustBeUniqueAmongSiblings
  deriving (Show, Eq)

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> Maybe CategoryId
  -> NonEmpty T.Text
  -> m (Either Failure Category)
run Handle {..} authUser parentCatId catNames = do
  authorize "create a category" $ authUserShouldBeAdmin authUser
  if any T.null catNames
    then pure $ Left CategoryNameMustNotBeEmpty
    else hCategoryIdWithParentAndNameExists parentCatId (NonEmpty.head catNames) >>= \case
           False -> first toFailure <$> hCreateCategory parentCatId catNames
           True -> pure $ Left CategoryNameMustBeUniqueAmongSiblings
  where
    toFailure CCFUnknownParentCategoryId = UnknownParentCategoryId
