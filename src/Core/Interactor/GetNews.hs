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
import qualified Data.List.NonEmpty as N
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
-- due to forcing to detect the special case. Treating an empty list
-- consistently results in no matching items and so it is useless.
newtype NewsFilter =
  NewsFilter
    { nfDateRanges :: Maybe (N.NonEmpty NewsDateRange)
    }

emptyNewsFilter :: NewsFilter
emptyNewsFilter = NewsFilter {nfDateRanges = Nothing}

-- | The inclusive range of dates.
data NewsDateRange
  = NewsSinceUntil Day Day
  | NewsSince Day
  | NewsUntil Day
  deriving (Eq, Show)

newtype GatewayNewsFilter =
  GatewayNewsFilter
    { gnfDateRanges :: Maybe (N.NonEmpty NewsDateRange)
    }

gatewayNewsFilterFromNewsFilter :: NewsFilter -> GatewayNewsFilter
gatewayNewsFilterFromNewsFilter NewsFilter {..} =
  GatewayNewsFilter {gnfDateRanges = nfDateRanges}
