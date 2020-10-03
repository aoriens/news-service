module Gateway.Categories
  ( createCategory
  , getCategory
  , getCategories
  ) where

import Core.Category
import qualified Core.Interactor.CreateCategory as CreateCategory
import Core.Pagination
import Data.List.NonEmpty
import Data.Text (Text)
import Database
import qualified Database.Categories as DCategories

createCategory ::
     Database.Handle
  -> Maybe CategoryId
  -> NonEmpty Text
  -> IO (Either CreateCategory.CreateCategoryFailure Category)
createCategory h parentId =
  runTransactionRW h . DCategories.createCategory parentId

getCategory :: Database.Handle -> CategoryId -> IO (Maybe Category)
getCategory h = runTransaction h . statement DCategories.selectCategory

getCategories :: Database.Handle -> PageSpec -> IO [Category]
getCategories h = runTransaction h . statement DCategories.selectCategories
