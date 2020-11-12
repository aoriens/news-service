module Core.Interactor.GetNews
  ( getNews
  , Handle(..)
  , NewsFilter(..)
  , NewsDateRange(..)
  , emptyNewsFilter
  , GatewayNewsFilter(..)
  , GatewayNewsAuthorFilter(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.News
import Core.Pagination
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
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
-- considered as filters over specific news parameters. Filters over
-- independent news fields should be combined using logical AND: if a
-- news entry does not conform to any filter field, it should not be
-- issued. Filters over dependent fields (e.g. author id and author
-- name filters) should be combined using logical OR.
--
-- 'Maybe' is used to represent 'no filtering' value as Nothing. We
-- might use the empty list or set for it, but it might be error-prone
-- due to having to detect the special case. Handling an empty list
-- consistently results in no matching items and so it is useless.
data NewsFilter =
  NewsFilter
    { nfDateRanges :: Maybe (N.NonEmpty NewsDateRange)
    , nfAuthorIds :: Maybe (Set.HashSet AuthorId)
    , nfAuthorNames :: Maybe (Set.HashSet T.Text)
    }

emptyNewsFilter :: NewsFilter
emptyNewsFilter =
  NewsFilter
    {nfDateRanges = Nothing, nfAuthorIds = Nothing, nfAuthorNames = Nothing}

-- | The inclusive range of dates.
data NewsDateRange
  = NewsSinceUntil Day Day
  | NewsSince Day
  | NewsUntil Day
  deriving (Eq, Show)

-- | A news filter to pass to the gateway. All fields are filters
-- which should be combined using logical AND.
data GatewayNewsFilter =
  GatewayNewsFilter
    { gnfDateRanges :: Maybe (N.NonEmpty NewsDateRange)
    , gnfAuthorFilter :: GatewayNewsAuthorFilter
    }

-- | An author filter. Its fields correspond to filters that should be
-- combined using logical OR.
data GatewayNewsAuthorFilter =
  GatewayNewsAuthorFilter
    { gnfAuthorIds :: Maybe (Set.HashSet AuthorId)
    , gnfAuthorNames :: Maybe (Set.HashSet T.Text)
    }

gatewayNewsFilterFromNewsFilter :: NewsFilter -> GatewayNewsFilter
gatewayNewsFilterFromNewsFilter NewsFilter {..} =
  GatewayNewsFilter
    { gnfDateRanges = nfDateRanges
    , gnfAuthorFilter =
        GatewayNewsAuthorFilter
          {gnfAuthorIds = nfAuthorIds, gnfAuthorNames = nfAuthorNames}
    }
