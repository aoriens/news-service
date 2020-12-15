module Core.Interactor.CreateCategory
  ( run
  , Handle(..)
  , Failure(..)
  , CreateCategoryFailure(..)
  ) where

import Control.Monad.Catch
import Core.AuthorizationNG
import Core.Category
import Data.Bifunctor
import Data.List.NonEmpty
import qualified Data.Text as T

newtype Handle m =
  Handle
    { hCreateCategory :: Maybe CategoryId -> NonEmpty T.Text -> m (Either CreateCategoryFailure Category)
    }

data CreateCategoryFailure =
  CCFUnknownParentCategoryId
  deriving (Show, Eq)

data Failure
  = UnknownParentCategoryId
  | CategoryNameMustNotBeEmpty
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
    else first toFailure <$> hCreateCategory parentCatId catNames
  where
    toFailure CCFUnknownParentCategoryId = UnknownParentCategoryId
