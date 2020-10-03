{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Database.Categories
  ( createCategory
  , selectCategory
  , selectCategories
  ) where

import Control.Arrow
import Control.Monad
import Core.Category
import qualified Core.Interactor.CreateCategory as CreateCategory
import Core.Pagination
import Data.Foldable
import Data.Int
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Lazy as Map
import Data.Profunctor
import qualified Data.Text as T
import Database
import qualified Hasql.TH as TH

createCategory ::
     Maybe CategoryId
  -> NonEmpty T.Text
  -> Transaction (Either CreateCategory.CreateCategoryFailure Category)
createCategory Nothing names = Right <$> createCategoriesInRoot names
createCategory (Just parentId) names = do
  optParentCat <- statement selectCategory parentId
  case optParentCat of
    Just parentCat ->
      Right <$> createCategoriesInParent parentCat (toList names)
    Nothing -> pure $ Left CreateCategory.CCFUnknownParentCategoryId

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
      from categories join cats
           on categories.category_id = cats.parent_id
    )
    select category_id :: integer, name :: varchar
    from cats
    |]
  where
    foldToCategory = foldr f Nothing
    f (catId, categoryName) categoryParent =
      Just
        Category {categoryId = CategoryId catId, categoryName, categoryParent}

selectCategories :: Statement PageSpec [Category]
selectCategories =
  dimap
    (\PageSpec {..} -> (getPageLimit pageLimit, getPageOffset pageOffset))
    (categoriesFromRows . toList)
    [TH.vectorStatement|
      with recursive cats as (
        select *
        from (
          select true as isFinal, *
          from categories
          limit $1 :: integer offset $2 :: integer
        ) as final_cats

        union

        select false as isFinal, categories.*
        from categories join cats
             on categories.category_id = cats.parent_id
      )
      select isFinal :: bool, category_id :: integer, parent_id :: integer?, name :: varchar
      from cats
    |]

categoriesFromRows :: [(Bool, Int32, Maybe Int32, T.Text)] -> [Category]
categoriesFromRows rows = map toCategory finalRows
  where
    (finalRows, ancestorRows) = partition (\(isFinal, _, _, _) -> isFinal) rows
    toCategory (_, catId, optParentId, categoryName) =
      Category
        { categoryName
        , categoryId = CategoryId catId
        , categoryParent = (`Map.lookup` ancestorsMap) =<< optParentId
        }
    ancestorsMap = foldl' insertToMap Map.empty ancestorRows
    insertToMap amap row =
      let category = toCategory row
       in Map.insert (getCategoryId $ categoryId category) category amap
