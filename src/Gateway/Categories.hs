module Gateway.Categories
  ( createCategory
  , getCategory
  , getCategories
  , deleteCategory
  ) where

import Core.Category
import qualified Core.Interactor.CreateCategory as CreateCategory
import qualified Core.Interactor.DeleteCategory as DeleteCategory
import Core.Pagination
import Data.List.NonEmpty
import Data.Text (Text)
import qualified Database.Categories as DCategories
import Database.Service.Primitives as Database

createCategory ::
     Database.Handle
  -> Maybe CategoryId
  -> NonEmpty Text
  -> IO (Either CreateCategory.CreateCategoryFailure Category)
createCategory h parentId =
  runTransactionRW h . DCategories.createCategory parentId

getCategory :: Database.Handle -> CategoryId -> IO (Maybe Category)
getCategory h = runTransactionRO h . DCategories.selectCategory

getCategories :: Database.Handle -> PageSpec -> IO [Category]
getCategories h = runTransactionRO h . DCategories.selectCategories

deleteCategory ::
     Database.Handle
  -> CategoryId
  -> PageSpec
  -> IO (Either DeleteCategory.Failure ())
deleteCategory h = (runTransactionRW h .) . DCategories.deleteCategory
