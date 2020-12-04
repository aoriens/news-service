{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Database.Logic.Categories
  ( createCategory
  , selectCategory
  , selectCategories
  , setCategoryIdToNewsVersionsInCategoryAndDescendantCategories
  , deleteCategoryAndDescendants
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
import Database.Service.Primitives
import qualified Hasql.TH as TH

createCategory ::
     Maybe CategoryId
  -> NonEmpty T.Text
  -> Transaction (Either CreateCategory.CreateCategoryFailure Category)
createCategory Nothing names = Right <$> createCategoriesInRoot names
createCategory (Just parentId) names = do
  optParentCat <- selectCategory parentId
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
  categoryId <- insertCategorySt (categoryId <$> categoryParent) categoryName
  pure Category {..}

insertCategorySt :: Maybe CategoryId -> T.Text -> Transaction CategoryId
insertCategorySt =
  curry . runStatement $
  dimap
    (first (fmap getCategoryId))
    CategoryId
    [TH.singletonStatement|
      insert into categories (parent_id, name) values (
          $1 :: integer?,
          $2 :: varchar
      ) returning category_id :: integer
    |]

selectCategory :: CategoryId -> Transaction (Maybe Category)
selectCategory =
  runStatement $
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

selectCategories :: PageSpec -> Transaction [Category]
selectCategories =
  runStatement $
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

setCategoryIdToNewsVersionsInCategoryAndDescendantCategories ::
     Maybe CategoryId -> CategoryId -> Transaction ()
setCategoryIdToNewsVersionsInCategoryAndDescendantCategories {- newCatId oldCatId -}
 =
  curry . runStatement $
  lmap
    (fmap getCategoryId *** getCategoryId)
    [TH.resultlessStatement|
      with recursive descendant_cats as (
        select $2 :: integer

        union

        select categories.category_id
        from categories join descendant_cats
             on categories.category_id = descendant_cats.parent_id
      )
      update news_versions
      set category_id = $1 :: integer?
      where category_id = any(descendant_cats)
    |]

deleteCategoryAndDescendants :: CategoryId -> Transaction ()
deleteCategoryAndDescendants =
  runStatement $
  lmap
    getCategoryId
    [TH.resultlessStatement|
      with recursive descendant_cats as (
        select $1 :: integer

        union

        select categories.category_id
        from categories join descendant_cats
             on categories.category_id = descendant_cats.parent_id
      )
      delete from categories
      where category_id = any(descendant_cats)
    |]
