module Gateway.Tags
  ( findTagByName
  , findTagById
  , getTags
  , createTagNamed
  ) where

import Core.Pagination
import Core.Tag
import Data.Text (Text)
import Database.Service.Primitives as DB
import qualified Database.Tags as DTags

findTagByName :: DB.Handle -> Text -> IO (Maybe Tag)
findTagByName h = runTransactionRO h . DTags.findTagByName

findTagById :: DB.Handle -> TagId -> IO (Maybe Tag)
findTagById h = runTransactionRO h . DTags.findTagById

getTags :: DB.Handle -> PageSpec -> IO [Tag]
getTags h = runTransactionRO h . DTags.getTags

createTagNamed :: DB.Handle -> Text -> IO Tag
createTagNamed h = runTransactionRW h . DTags.createTagNamed
