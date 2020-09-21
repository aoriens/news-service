{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Database.Categories
  ( createCategory
  ) where

import Control.Arrow
import Control.Monad
import Core.Category
import qualified Core.Interactor.CreateCategory as CreateCategory
import Data.List.NonEmpty
import Data.Profunctor
import qualified Data.Text as T
import Database
import qualified Hasql.TH as TH

createCategory ::
     Maybe CategoryId
  -> NonEmpty T.Text
  -> Transaction (Either CreateCategory.Failure Category)
createCategory Nothing names = Right <$> createCategoriesInRoot names
createCategory (Just parentId) names = do
  optParentCat <- statement selectCategory parentId
  case optParentCat of
    Just parentCat ->
      Right <$> createCategoriesInParent parentCat (toList names)
    Nothing -> pure $ Left CreateCategory.UnknownParentCategoryId

createCategoriesInRoot :: NonEmpty T.Text -> Transaction Category
createCategoriesInRoot (name :| names) = do
  parent <- insertCategory Nothing name
  createCategoriesInParent parent names

createCategoriesInParent :: Category -> [T.Text] -> Transaction Category
createCategoriesInParent = foldM (insertCategory . Just)

insertCategory :: Maybe Category -> T.Text -> Transaction Category
insertCategory categoryParent categoryName = do
  categoryId <-
    statement insertCategorySt (categoryId <$> categoryParent, categoryName)
  pure Category {..}

insertCategorySt :: Statement (Maybe CategoryId, T.Text) CategoryId
insertCategorySt =
  dimap
    (first (fmap getCategoryId))
    CategoryId
    [TH.singletonStatement|
    insert into categories (parent_id, name) values (
        $1 :: integer?,
        $2 :: varchar
    ) returning category_id :: integer
    |]

selectCategory :: Statement CategoryId (Maybe Category)
selectCategory =
  dimap
    getCategoryId
    foldToCategory
    [TH.vectorStatement|
    with recursive cats as (
      select *
      from categories
      where category_id = $1 :: integer

      union

      select categories.*
      from categories join cats on cats.parent_id = categories.category_id
    )
    select category_id :: integer, name :: varchar
    from cats
    |]
  where
    foldToCategory = foldr f Nothing
    f (catId, categoryName) categoryParent =
      Just Category {categoryId = CategoryId catId, ..}
