module Gateway.Tags
  ( findTagByName
  , findTagById
  , createTagNamed
  ) where

import Core.Tag
import Data.Text (Text)
import Database
import qualified Database.Tags as DTags

findTagByName :: Database.Handle -> Text -> IO (Maybe Tag)
findTagByName h = runTransaction h . statement DTags.findTagByName

findTagById :: Database.Handle -> TagId -> IO (Maybe Tag)
findTagById h = runTransaction h . statement DTags.findTagById

createTagNamed :: Database.Handle -> Text -> IO Tag
createTagNamed h = runTransactionRW h . DTags.createTagNamed
