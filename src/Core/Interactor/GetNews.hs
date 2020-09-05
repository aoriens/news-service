module Core.Interactor.GetNews
  ( getNews
  , Handle(..)
  , News(..)
  ) where

import Control.Monad.Catch
import Core.Pagination
import Data.Int
import Data.Text (Text)
import Data.Time.Calendar

getNews :: MonadThrow m => Handle m -> PageQuery -> m [News]
getNews Handle {..} pageQuery =
  hGetNews =<< pageFromPageQueryM hMaxPageLimit pageQuery

data Handle m =
  Handle
    { hGetNews :: Page -> m [News]
    , hMaxPageLimit :: PageLimit
    }

data News =
  News
    { newsId :: Int32
    , newsTitle :: Text
    , newsDate :: Day
    , newsText :: Text
    }
  deriving (Eq, Show)
