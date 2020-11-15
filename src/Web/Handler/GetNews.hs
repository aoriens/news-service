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
import Web.QueryParameter
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
  (pageQuery, newsFilter) <-
    parseQueryM (requestQueryString request) parseParams
  news <- I.getNews hGetNewsHandle newsFilter pageQuery
  respond $ hPresenter news

parseParams :: QueryParser (PageSpecQuery, I.NewsFilter)
parseParams = liftA2 (,) parsePageQuery parseNewsFilter

parseNewsFilter :: QueryParser I.NewsFilter
parseNewsFilter = do
  dateRanges <- fmap getDateRange <$> collectQueryParameter "date"
  authorIds <-
    map AuthorId . concatMap getCommaSeparatedList <$>
    collectQueryParameter "author_id"
  authorNameSubstrings <- collectQueryParameter "author"
  categoryIds <-
    map CategoryId . concatMap getCommaSeparatedList <$>
    collectQueryParameter "category_id"
  categoryNameSubstrings <- collectQueryParameter "category"
  anyTagIds <-
    map TagId . concatMap getCommaSeparatedList <$>
    collectQueryParameter "tag_id"
  anyTagNameSubstrings <- collectQueryParameter "tag"
  requiredTagIds <-
    map TagId . concatMap getCommaSeparatedList <$>
    collectQueryParameter "required_tag_id"
  requiredTagNameSubstrings <- collectQueryParameter "required_tag"
  titleSubstrings <- collectQueryParameter "title"
  bodySubstrings <- collectQueryParameter "body"
  substringEverywhere <- lookupQueryParameter "q"
  pure
    I.NewsFilter
      { nfDateRanges = N.nonEmpty dateRanges
      , nfAuthorIds = nonEmptySet authorIds
      , nfAuthorNameSubstrings = nonEmptySet authorNameSubstrings
      , nfCategoryIds = nonEmptySet categoryIds
      , nfCategoryNameSubstrings = nonEmptySet categoryNameSubstrings
      , nfTagIdsToMatchAnyTag = nonEmptySet anyTagIds
      , nfTagNameSubstringsToMatchAnyTag = nonEmptySet anyTagNameSubstrings
      , nfTagIdsAllRequiredToMatch = nonEmptySet requiredTagIds
      , nfTagNameSubstringsAllRequiredToMatch =
          nonEmptySet requiredTagNameSubstrings
      , nfTitleSubstrings = nonEmptySet titleSubstrings
      , nfBodySubstrings = nonEmptySet bodySubstrings
      , nfSubstringsAnywhere = Set.singleton <$> substringEverywhere
      }

nonEmptySet :: (Eq a, Hashable a) => [a] -> Maybe (Set.HashSet a)
nonEmptySet xs
  | null xs = Nothing
  | otherwise = Just $ Set.fromList xs

newtype DateRange =
  DateRange
    { getDateRange :: I.NewsDateRange
    }

instance QueryParameter DateRange where
  parseQueryParameter = fmap DateRange . parseDateRange

parseDateRange :: Maybe B.ByteString -> Maybe I.NewsDateRange
parseDateRange str =
  case map T.unpack . getCommaSeparatedList <$> parseQueryParameter str of
    Just [s] -> (\day -> I.NewsSinceUntil day day) <$> iso8601ParseM s
    Just ["", s2] -> I.NewsUntil <$> iso8601ParseM s2
    Just [s1, ""] -> I.NewsSince <$> iso8601ParseM s1
    Just [s1, s2] ->
      liftA2 I.NewsSinceUntil (iso8601ParseM s1) (iso8601ParseM s2)
    _ -> Nothing
