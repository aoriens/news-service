{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}

module Web.Handler.GetNews
  ( Handle(..)
  , run
  ) where

import Control.Applicative
import Core.Author
import Core.Category
import qualified Core.Interactor.GetNews as I
import Core.News
import Core.Pagination
import Core.Tag
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as Set
import Data.Hashable
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import Data.Time.Format.ISO8601
import Web.Application
import qualified Web.QueryParameter as QueryParameter
import Web.QueryParameter.PageQuery

data Handle =
  Handle
    { hGetNewsHandle :: I.Handle IO
    , hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hPresenter :: [News] -> Response
    }

run :: Handle -> Application
run Handle {..} request respond = do
  (pageQuery, newsFilter, sortOptions) <-
    QueryParameter.parseQueryM (requestQueryString request) parseParams
  news <- I.getNews hGetNewsHandle newsFilter sortOptions pageQuery
  respond $ hPresenter news

parseParams :: QueryParameter.Parser (PageSpecQuery, I.Filter, I.SortOptions)
parseParams = liftA3 (,,) parsePageQuery parseFilter parseSortOptions

parseFilter :: QueryParameter.Parser I.Filter
parseFilter = do
  dateRanges <- fmap getDateRange <$> QueryParameter.collect "date"
  authorIds <-
    map AuthorId . concatMap QueryParameter.getCommaSeparatedList <$>
    QueryParameter.collect "author_id"
  authorNameSubstrings <- QueryParameter.collect "author"
  categoryIds <-
    map CategoryId . concatMap QueryParameter.getCommaSeparatedList <$>
    QueryParameter.collect "category_id"
  categoryNameSubstrings <- QueryParameter.collect "category"
  anyTagIds <-
    map TagId . concatMap QueryParameter.getCommaSeparatedList <$>
    QueryParameter.collect "tag_id"
  anyTagNameSubstrings <- QueryParameter.collect "tag"
  requiredTagIds <-
    map TagId . concatMap QueryParameter.getCommaSeparatedList <$>
    QueryParameter.collect "required_tag_id"
  requiredTagNameSubstrings <- QueryParameter.collect "required_tag"
  titleSubstrings <- QueryParameter.collect "title"
  bodySubstrings <- QueryParameter.collect "body"
  substringEverywhere <- QueryParameter.lookup "q"
  pure
    I.Filter
      { fDateRanges = N.nonEmpty dateRanges
      , fAuthorIds = nonEmptySet authorIds
      , fAuthorNameSubstrings = nonEmptySet authorNameSubstrings
      , fCategoryIds = nonEmptySet categoryIds
      , fCategoryNameSubstrings = nonEmptySet categoryNameSubstrings
      , fTagIdsToMatchAnyTag = nonEmptySet anyTagIds
      , fTagNameSubstringsToMatchAnyTag = nonEmptySet anyTagNameSubstrings
      , fTagIdsAllRequiredToMatch = nonEmptySet requiredTagIds
      , fTagNameSubstringsAllRequiredToMatch =
          nonEmptySet requiredTagNameSubstrings
      , fTitleSubstrings = nonEmptySet titleSubstrings
      , fBodySubstrings = nonEmptySet bodySubstrings
      , fSubstringsAnywhere = Set.singleton <$> substringEverywhere
      }

nonEmptySet :: (Eq a, Hashable a) => [a] -> Maybe (Set.HashSet a)
nonEmptySet xs
  | null xs = Nothing
  | otherwise = Just $ Set.fromList xs

newtype DateRange =
  DateRange
    { getDateRange :: I.NewsDateRange
    }

instance QueryParameter.Parses DateRange where
  parse = fmap DateRange . parseDateRange

parseDateRange :: Maybe B.ByteString -> Maybe I.NewsDateRange
parseDateRange str =
  case map T.unpack . QueryParameter.getCommaSeparatedList <$>
       QueryParameter.parse str of
    Just [s] -> (\day -> I.NewsSinceUntil day day) <$> iso8601ParseM s
    Just ["", s2] -> I.NewsUntil <$> iso8601ParseM s2
    Just [s1, ""] -> I.NewsSince <$> iso8601ParseM s1
    Just [s1, s2] ->
      liftA2 I.NewsSinceUntil (iso8601ParseM s1) (iso8601ParseM s2)
    _ -> Nothing

parseSortOptions :: QueryParameter.Parser I.SortOptions
parseSortOptions = do
  sortReverse <- QueryParameter.exists "reverse_sort"
  sortKey <-
    maybe (I.sortKey I.defaultSortOptions) unwrapSortKey <$>
    QueryParameter.lookup "sort"
  pure I.SortOptions {sortReverse, sortKey}

newtype SortKey =
  SortKey
    { unwrapSortKey :: I.SortKey
    }

instance QueryParameter.Parses SortKey where
  parse (Just "date") = Just $ SortKey I.SortKeyDate
  parse (Just "author") = Just $ SortKey I.SortKeyAuthorName
  parse _ = Nothing
