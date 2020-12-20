{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}

module Database.Logic.Categories
  ( createCategory
  , selectCategory
  , categoryExists
  , selectCategories
  , setCategoryIdToNewsVersionsInCategoryAndDescendantCategories
  , deleteCategoryAndDescendants
  , updateCategory
  , getCategoryIdBySiblingAndName
  , getCategoryIdByParentAndName
  , categoryIsDescendantOf
  , getCategoryName
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Core.Category
import qualified Core.Interactor.CreateCategory as CreateCategory
import qualified Core.Interactor.UpdateCategory as UpdateCategory
import Core.Pagination
import Data.Foldable
import Data.Functor.Contravariant
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Profunctor
import qualified Data.Text as T
import Database.Logic.Pagination
import Database.Service.Columns
import Database.Service.Primitives
import Database.Service.SQLBuilder as Sql
import Database.Service.SQLBuilders as Sql
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
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
  fmap (listToMaybe . categoriesFromRows . toList) . runStatement statement
  where
    statement =
      statementWithColumns sql encoder categoryRowColumns D.rowVector True
    sql =
      [TH.uncheckedSql|
        with recursive cats as (
          select true as is_final, *
          from categories
          where category_id = $1

          union

          select false as is_final, categories.*
          from categories join cats
               on categories.category_id = cats.parent_id
        )
        select $COLUMNS
        from cats
      |]
    encoder = getCategoryId >$< E.param (E.nonNullable E.int4)

selectCategories :: PageSpec -> Transaction [Category]
selectCategories = fmap (categoriesFromRows . toList) . runStatement statement
  where
    statement =
      statementWithColumns sql encoder categoryRowColumns D.rowVector True
    sql =
      [TH.uncheckedSql|
        with recursive cats as (
          select *
          from (
            select true as is_final, *
            from categories
            limit $1 offset $2
          ) as final_rows

          union

          select false as is_final, categories.*
          from categories join cats
               on categories.category_id = cats.parent_id
        )
        select $COLUMNS
        from cats
      |]
    encoder = pageToLimitOffsetEncoder

data CategoryRow =
  CategoryRow
    { rowId :: CategoryId
    , rowParentId :: Maybe CategoryId
    , rowName :: T.Text
    , rowIsFinal :: Bool
    -- ^ Whether the row corresponds to the final, requested category,
    -- as opposed to an intermediate ancestor category. This is a fake
    -- column, constructed in a request.
    }

categoryRowColumns :: Columns CategoryRow
categoryRowColumns = do
  rowId <- CategoryId <$> column noTable "category_id"
  rowParentId <- fmap CategoryId <$> column noTable "parent_id"
  rowName <- column noTable "name"
  rowIsFinal <- column noTable "is_final"
  pure CategoryRow {rowId, rowParentId, rowName, rowIsFinal}

categoriesFromRows :: [CategoryRow] -> [Category]
categoriesFromRows rows = map toCategory finalRows
  where
    (finalRows, ancestorRows) = partition rowIsFinal rows
    toCategory CategoryRow {..} =
      Category
        { categoryName = rowName
        , categoryId = rowId
        , categoryParent = (`Map.lookup` ancestorsMap) =<< rowParentId
        }
    ancestorsMap = foldl' insertToMap Map.empty ancestorRows
    insertToMap amap row =
      let category = toCategory row
       in Map.insert (categoryId category) category amap

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

updateCategory ::
     UpdateCategory.Request
  -> Transaction (Either UpdateCategory.UpdateCategoryFailure Category)
updateCategory r@UpdateCategory.Request {..} =
  runExceptT $ do
    forM_ (join rNewParent) $
      lift . categoryExists >=>
      (`unless` throwE UpdateCategory.UCUnknownNewParentId)
    lift (uncheckedUpdateCategory r) >>=
      (`unless` throwE UpdateCategory.UCUnknownCategoryId)
    lift (selectCategory rCategoryId) >>= \case
      Nothing -> throwE UpdateCategory.UCUnknownCategoryId
      Just cat -> pure cat

categoryExists :: CategoryId -> Transaction Bool
categoryExists =
  runStatement $
  lmap
    getCategoryId
    [TH.singletonStatement|
      select exists
        (select 1
         from categories
         where category_id = $1 :: integer) :: bool
    |]

uncheckedUpdateCategory :: UpdateCategory.Request -> Transaction Bool
uncheckedUpdateCategory UpdateCategory.Request {..}
  | Sql.isEmpty updateClauses = pure True
  | otherwise = (> 0) <$> Sql.runBuilder D.rowsAffected True sql
  where
    sql =
      "update categories set" <>
      updateClauses <>
      "where category_id =" <> Sql.param (getCategoryId rCategoryId)
    updateClauses = Sql.csv $ catMaybes [updateNameSql, updateParentSql]
    updateNameSql = ("name =" <>) . Sql.param <$> rNewName
    updateParentSql =
      ("parent_id =" <>) . Sql.param . fmap getCategoryId <$> rNewParent

getCategoryIdBySiblingAndName ::
     CategoryId -> T.Text -> Transaction (Maybe CategoryId)
getCategoryIdBySiblingAndName =
  curry . runStatement $
  dimap
    (first getCategoryId)
    (fmap CategoryId)
    [TH.maybeStatement|
      select category_id :: integer
      from categories
      where name = $2 :: varchar
            and parent_id =
                (select parent_id
                 from categories
                 where category_id = $1 :: integer)
      limit 1
    |]

getCategoryIdByParentAndName ::
     Maybe CategoryId -> T.Text -> Transaction (Maybe CategoryId)
getCategoryIdByParentAndName =
  curry . runStatement $
  dimap
    (first $ fmap getCategoryId)
    (fmap CategoryId)
    [TH.maybeStatement|
      select category_id :: integer
      from categories
      where parent_id is not distinct from $1 :: integer? and name = $2 :: varchar
      limit 1
    |]

-- | Determines whether the first category is a descendant of the second.
categoryIsDescendantOf :: CategoryId -> CategoryId -> Transaction Bool
categoryIsDescendantOf =
  curry . runStatement $
  lmap
    (getCategoryId *** getCategoryId)
    [TH.singletonStatement|
      with recursive ancestors as (
        select $1 :: integer as category_id

        union

        select parent_id
        from categories
             join ancestors using (category_id)
        where parent_id is not null
      )
      select ($2 :: integer = any (select * from ancestors)) :: bool
    |]

getCategoryName :: CategoryId -> Transaction (Maybe T.Text)
getCategoryName =
  runStatement $
  lmap
    getCategoryId
    [TH.maybeStatement|
      select name :: varchar
      from categories
      where category_id = $1 :: integer
    |]
