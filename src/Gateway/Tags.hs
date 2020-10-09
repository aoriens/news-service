module Gateway.Tags
  ( findTagByName
  , findTagById
  , getTags
  , createTagNamed
  ) where

import Core.Pagination
import Core.Tag
import Data.Text (Text)
import Database
import qualified Database.Tags as DTags

findTagByName :: Database.Handle -> Text -> IO (Maybe Tag)
findTagByName h = runTransaction h . statement DTags.findTagByName

findTagById :: Database.Handle -> TagId -> IO (Maybe Tag)
findTagById h = runTransaction h . statement DTags.findTagById

getTags :: Database.Handle -> PageSpec -> IO [Tag]
getTags h = runTransaction h . statement DTags.getTags

createTagNamed :: Database.Handle -> Text -> IO Tag
createTagNamed h = runTransactionRW h . DTags.createTagNamed