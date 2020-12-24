module Core.Interactor.DeleteCategory
  ( run
  , Handle(..)
  , Failure(..)
  ) where

import Control.Monad.Catch
import Core.Authorization
import Core.Category

data Handle m =
  Handle
    { hGetCategory :: CategoryId -> m (Maybe Category)
    , hSetCategoryIdToNewsVersionsInCategoryAndDescendantCategories :: Maybe CategoryId -> CategoryId -> m ()
    , hDeleteCategoryAndDescendants :: CategoryId -> m ()
    }

run ::
     MonadThrow m
  => Handle m
  -> AuthenticatedUser
  -> CategoryId
  -> m (Either Failure ())
run Handle {..} authUser catId = do
  authorize "delete a category" $ authUserShouldBeAdmin authUser
  hGetCategory catId >>= \case
    Nothing -> pure $ Left UnknownCategoryId
    Just cat -> do
      hSetCategoryIdToNewsVersionsInCategoryAndDescendantCategories
        (categoryId <$> categoryParent cat)
        catId
      hDeleteCategoryAndDescendants catId
      pure $ Right ()

data Failure =
  UnknownCategoryId
  deriving (Eq, Show)
