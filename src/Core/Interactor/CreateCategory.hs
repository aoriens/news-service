module Core.Interactor.CreateCategory
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Category
import Data.List.NonEmpty
import qualified Data.Text as T

data Handle m =
  Handle
    { hAuthHandle :: AuthenticationHandle m
    , hCreateCategory :: Maybe CategoryId -> NonEmpty T.Text -> m (Either Failure Category)
    , hAuthorizationHandle :: AuthorizationHandle
    }

data Failure =
  UnknownParentCategoryId

run ::
     MonadThrow m
  => Handle m
  -> Maybe Credentials
  -> Maybe CategoryId
  -> NonEmpty T.Text
  -> m (Either Failure Category)
run Handle {..} creds parentCatId catName = do
  actor <- authenticate hAuthHandle creds
  requireAdminPermission hAuthorizationHandle actor "create category"
  hCreateCategory parentCatId catName
