module Core.Interactor.GetNews
  ( getNews
  , Handle(..)
  , NewsFilter(..)
  , NewsDateRange(..)
  , emptyNewsFilter
  , GatewayNewsFilter(..)
  ) where

import Control.Monad.Catch
import Core.News
import Core.Pagination
import Data.Time

getNews :: MonadThrow m => Handle m -> NewsFilter -> PageSpecQuery -> m [News]
getNews Handle {..} newsFilter pageQuery = do
  pageSpec <- parsePageSpecM hPageSpecParserHandle pageQuery
  hGetNews (gatewayNewsFilterFromNewsFilter newsFilter) pageSpec

data Handle m =
  Handle
    { hGetNews :: GatewayNewsFilter -> PageSpec -> m [News]
    , hPageSpecParserHandle :: PageSpecParserHandle
    }

-- | A specification of a subset of news entries. All fields are
-- considered as filters over specific news parameters. They should be
-- combined using logical AND: if a news entry does not conform to any
-- filter field, it should not be issued.
--
-- 'Maybe' is used to represent 'no filtering' value as Nothing. We
-- might use the empty list or set for it, but it might be error-prone
-- and involve detecting that special case, since processing an empty
-- list consistently results in no matching items.
newtype NewsFilter =
  NewsFilter
    { nfDateRange :: Maybe NewsDateRange
    }

emptyNewsFilter :: NewsFilter
emptyNewsFilter = NewsFilter {nfDateRange = Nothing}

-- | The inclusive range of dates.
data NewsDateRange
  = NewsSinceUntil Day Day
  | NewsSince Day
  | NewsUntil Day

newtype GatewayNewsFilter =
  GatewayNewsFilter
    { gnfDateRange :: Maybe NewsDateRange
    }

gatewayNewsFilterFromNewsFilter :: NewsFilter -> GatewayNewsFilter
gatewayNewsFilterFromNewsFilter NewsFilter {..} =
  GatewayNewsFilter {gnfDateRange = nfDateRange}
