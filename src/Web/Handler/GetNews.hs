{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}

module Web.Handler.GetNews
  ( Handle(..)
  , run
  ) where

import Control.Applicative
import Core.Author
import qualified Core.Interactor.GetNews as I
import Core.News
import Core.Pagination
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Util as B
import qualified Data.HashSet as Set
import Data.Hashable
import qualified Data.List.NonEmpty as N
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
  authorNames <- collectQueryParameter "author"
  pure
    I.NewsFilter
      { nfDateRanges = N.nonEmpty dateRanges
      , nfAuthorIds = nonEmptySet authorIds
      , nfAuthorNames = nonEmptySet authorNames
      }

nonEmptySet :: (Eq a, Hashable a) => [a] -> Maybe (Set.HashSet a)
nonEmptySet [] = Nothing
nonEmptySet xs@(_:_) = Just $ Set.fromList xs

newtype DateRange =
  DateRange
    { getDateRange :: I.NewsDateRange
    }

instance QueryParameter DateRange where
  parseQueryParameter = (fmap DateRange . parseDateRange =<<)

parseDateRange :: B.ByteString -> Maybe I.NewsDateRange
parseDateRange str =
  case B.splitOnCharOnce (== ',') str of
    Nothing ->
      (\day -> I.NewsSinceUntil day day) <$> iso8601ParseM (B.unpack str)
    Just (from, to) ->
      case ( iso8601ParseM $ B.unpack from
           , B.null from
           , iso8601ParseM $ B.unpack to
           , B.null to) of
        (Just day1, _, Just day2, _) -> Just $ I.NewsSinceUntil day1 day2
        (Just day1, _, Nothing, True) -> Just $ I.NewsSince day1
        (Nothing, True, Just day2, _) -> Just $ I.NewsUntil day2
        _ -> Nothing
