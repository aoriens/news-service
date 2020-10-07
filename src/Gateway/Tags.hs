module Gateway.Tags
  ( findTagByName
  , createTagNamed
  ) where

import Core.Tag
import Data.Text (Text)
import Database
import qualified Database.Tags as DTags

findTagByName :: Database.Handle -> Text -> IO (Maybe Tag)
findTagByName h = runTransaction h . statement DTags.findTagByName

createTagNamed :: Database.Handle -> Text -> IO Tag
createTagNamed h = runTransactionRW h . DTags.createTagNamed
