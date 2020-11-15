module Core.Interactor.GetNews
  ( getNews
  , Handle(..)
  , Filter(..)
  , NewsDateRange(..)
  , emptyFilter
  , GatewayFilter(..)
  , GatewayAuthorFilter(..)
  , GatewayCategoryFilter(..)
  , GatewayAnyTagFilter(..)
  , GatewayAllTagsFilter(..)
  ) where

import Control.Monad.Catch
import Core.Author
import Core.Category
import Core.News
import Core.Pagination
import Core.Tag
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import Data.Time

getNews :: MonadThrow m => Handle m -> Filter -> PageSpecQuery -> m [News]
getNews Handle {..} newsFilter pageQuery = do
  pageSpec <- parsePageSpecM hPageSpecParserHandle pageQuery
  hGetNews (gatewayFilterFromFilter newsFilter) pageSpec

data Handle m =
  Handle
    { hGetNews :: GatewayFilter -> PageSpec -> m [News]
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
data Filter =
  Filter
    { fDateRanges :: Maybe (N.NonEmpty NewsDateRange)
    , fAuthorIds :: Maybe (Set.HashSet AuthorId)
    , fAuthorNameSubstrings :: Maybe (Set.HashSet T.Text)
    , fCategoryIds :: Maybe (Set.HashSet CategoryId)
    , fCategoryNameSubstrings :: Maybe (Set.HashSet T.Text)
    , fTagIdsToMatchAnyTag :: Maybe (Set.HashSet TagId)
    , fTagNameSubstringsToMatchAnyTag :: Maybe (Set.HashSet T.Text)
    , fTagIdsAllRequiredToMatch :: Maybe (Set.HashSet TagId)
    , fTagNameSubstringsAllRequiredToMatch :: Maybe (Set.HashSet T.Text)
    , fTitleSubstrings :: Maybe (Set.HashSet T.Text)
    , fBodySubstrings :: Maybe (Set.HashSet T.Text)
    , fSubstringsAnywhere :: Maybe (Set.HashSet T.Text)
    }

emptyFilter :: Filter
emptyFilter =
  Filter
    { fDateRanges = Nothing
    , fAuthorIds = Nothing
    , fAuthorNameSubstrings = Nothing
    , fCategoryIds = Nothing
    , fCategoryNameSubstrings = Nothing
    , fTagIdsToMatchAnyTag = Nothing
    , fTagNameSubstringsToMatchAnyTag = Nothing
    , fTagIdsAllRequiredToMatch = Nothing
    , fTagNameSubstringsAllRequiredToMatch = Nothing
    , fTitleSubstrings = Nothing
    , fBodySubstrings = Nothing
    , fSubstringsAnywhere = Nothing
    }

-- | The inclusive range of dates.
data NewsDateRange
  = NewsSinceUntil Day Day
  | NewsSince Day
  | NewsUntil Day
  deriving (Eq, Show)

-- | A news filter to pass to the gateway. All fields are filters
-- which should be combined using logical AND. The type is intended
-- for using in gateways and, therefore, it should not involve obscure
-- processing rules, so OR-combined fields must not present here, they
-- should be placed within other types.
data GatewayFilter =
  GatewayFilter
    { gfDateRanges :: Maybe (N.NonEmpty NewsDateRange)
    , gfAuthorFilter :: GatewayAuthorFilter
    , gfCategoryFilter :: GatewayCategoryFilter
    , gfAnyTagFilter :: GatewayAnyTagFilter
    , gfAllTagsFilter :: GatewayAllTagsFilter
    , gfTitleSubstrings :: Maybe (Set.HashSet T.Text)
    , gfBodySubstrings :: Maybe (Set.HashSet T.Text)
    , gfSubstringsAnywhere :: Maybe (Set.HashSet T.Text)
    }

-- | An author filter. Its fields correspond to filters that should be
-- combined using logical OR.
data GatewayAuthorFilter =
  GatewayAuthorFilter
    { gfAuthorIds :: Maybe (Set.HashSet AuthorId)
    , gfAuthorNameSubstrings :: Maybe (Set.HashSet T.Text)
    -- ^ Author name substrings to match with. Each element is a
    -- substring to be searched in the user name of an author of a
    -- news entry. The user name consists of the first name and the
    -- last name, separated with a space.
    }

-- | A category filter. Its fields correspond to filters that should
-- be combined using logical OR.
data GatewayCategoryFilter =
  GatewayCategoryFilter
    { gfCategoryIds :: Maybe (Set.HashSet CategoryId)
    -- ^ Category identifier to match with. A category matches to an
    -- identifier if its identifier or its ancestor category
    -- identifier matches.
    , gfCategoryNameSubstrings :: Maybe (Set.HashSet T.Text)
    -- ^ Category name substrings to match with. Each element is a
    -- substring to be searched in the category name or its ancestor
    -- category names.
    }

-- | A tag filter. It selects news entries matching at least one tag
-- specified. Its fields correspond to filters that should be combined
-- using logical OR.
data GatewayAnyTagFilter =
  GatewayAnyTagFilter
    { gfTagIdsToMatchAnyTag :: Maybe (Set.HashSet TagId)
    , gfTagNameSubstringsToMatchAnyTag :: Maybe (Set.HashSet T.Text)
    -- ^ Tag name substrings to match with. Each element is a
    -- substring to be searched in the tag name.
    }

-- | A tag filter. It selects news entries matching all tags specified. Its
-- fields correspond to filters that should be combined using logical
-- OR.
data GatewayAllTagsFilter =
  GatewayAllTagsFilter
    { gfTagIdsAllRequiredToMatch :: Maybe (Set.HashSet TagId)
    , gfTagNameSubstringsAllRequiredToMatch :: Maybe (Set.HashSet T.Text)
    -- ^ Tag name substrings to match with. Each element is a
    -- substring to be searched in the tag name.
    }

gatewayFilterFromFilter :: Filter -> GatewayFilter
gatewayFilterFromFilter Filter {..} =
  GatewayFilter
    { gfDateRanges = fDateRanges
    , gfTitleSubstrings = excludeEmptyString fTitleSubstrings
    , gfBodySubstrings = excludeEmptyString fBodySubstrings
    , gfSubstringsAnywhere = excludeEmptyString fSubstringsAnywhere
    , gfAuthorFilter =
        GatewayAuthorFilter
          { gfAuthorIds = fAuthorIds
          , gfAuthorNameSubstrings = excludeEmptyString fAuthorNameSubstrings
          }
    , gfCategoryFilter =
        GatewayCategoryFilter
          { gfCategoryIds = fCategoryIds
          , gfCategoryNameSubstrings =
              excludeEmptyString fCategoryNameSubstrings
          }
    , gfAnyTagFilter =
        GatewayAnyTagFilter
          { gfTagIdsToMatchAnyTag = fTagIdsToMatchAnyTag
          , gfTagNameSubstringsToMatchAnyTag =
              excludeEmptyString fTagNameSubstringsToMatchAnyTag
          }
    , gfAllTagsFilter =
        GatewayAllTagsFilter
          { gfTagIdsAllRequiredToMatch = fTagIdsAllRequiredToMatch
          , gfTagNameSubstringsAllRequiredToMatch =
              excludeEmptyString fTagNameSubstringsAllRequiredToMatch
          }
    }

-- | Excludes the empty string from a set of strings, normalizing to a
-- no-op filter when needed. It is a little optimization for hGetNews,
-- since an empty substring filter cannot filter out entries and it is
-- useless.
excludeEmptyString :: Maybe (Set.HashSet T.Text) -> Maybe (Set.HashSet T.Text)
excludeEmptyString = (normalize . Set.delete "" =<<)
  where
    normalize set
      | Set.null set = Nothing
      | otherwise = Just set
