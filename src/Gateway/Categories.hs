module Gateway.Categories
  ( createCategory
  , getCategory
  , getCategories
  , deleteCategory
  ) where

import Core.Category
import qualified Core.Interactor.CreateCategory as ICreateCategory
import qualified Core.Interactor.DeleteCategory as IDeleteCategory
import Core.Pagination
import Data.List.NonEmpty
import Data.Text (Text)
import qualified Database.Categories as DCategories
import Database.Service.Primitives as DB

createCategory ::
     DB.Handle
  -> Maybe CategoryId
  -> NonEmpty Text
  -> IO (Either ICreateCategory.CreateCategoryFailure Category)
createCategory h parentId =
  runTransactionRW h . DCategories.createCategory parentId

getCategory :: DB.Handle -> CategoryId -> IO (Maybe Category)
getCategory h = runTransactionRO h . DCategories.selectCategory

getCategories :: DB.Handle -> PageSpec -> IO [Category]
getCategories h = runTransactionRO h . DCategories.selectCategories

deleteCategory ::
     DB.Handle
  -> CategoryId
  -> PageSpec
  -> IO (Either IDeleteCategory.Failure ())
deleteCategory h = (runTransactionRW h .) . DCategories.deleteCategory
