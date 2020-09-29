module Gateway.Categories
  ( createCategory
  , getCategory
  ) where

import Core.Category
import qualified Core.Interactor.CreateCategory as CreateCategory
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
