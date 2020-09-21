module Gateway.Categories
  ( createCategory
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
  -> IO (Either CreateCategory.Failure Category)
createCategory h parentId =
  runTransactionRW h . DCategories.createCategory parentId
