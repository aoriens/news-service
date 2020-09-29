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
import Data.List.NonEmpty
import qualified Data.Text as T

data Handle m =
  Handle
    { hAuthHandle :: AuthenticationHandle m
    , hCreateCategory :: Maybe CategoryId -> NonEmpty T.Text -> m (Either CreateCategoryFailure Category)
    , hAuthorizationHandle :: AuthorizationHandle
    }

data CreateCategoryFailure =
  CCFUnknownParentCategoryId
  deriving (Show, Eq)

data Failure
  = UnknownParentCategoryId
  | IncorrectParameter Reason
  deriving (Show, Eq)

type Reason = T.Text

run ::
     MonadThrow m
  => Handle m
  -> Maybe Credentials
  -> Maybe CategoryId
  -> NonEmpty T.Text
  -> m (Either Failure Category)
run Handle {..} creds parentCatId catNames
  | any T.null catNames =
    pure . Left $ IncorrectParameter "Category name must not be empty"
  | otherwise = do
    actor <- authenticate hAuthHandle creds
    requireAdminPermission hAuthorizationHandle actor "create category"
    first toFailure <$> hCreateCategory parentCatId catNames
  where
    toFailure CCFUnknownParentCategoryId = UnknownParentCategoryId
