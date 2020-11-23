module Database.Logic.News.Filter
  ( whereClauseToFilterNews
  ) where

import Core.Author
import Core.Category
import qualified Core.Interactor.GetNewsList as IListNews
import Core.Tag
import Data.Foldable
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Database.Service.SQLBuilder as Sql
import qualified Database.Service.SQLBuilders as Sql

whereClauseToFilterNews :: IListNews.GatewayFilter -> Sql.Builder
whereClauseToFilterNews IListNews.GatewayFilter {..} =
  Sql.mapNonEmpty ("where" <>) $
  dateCondition gfDateRanges `Sql.and` authorCondition gfAuthorFilter `Sql.and`
  categoryCondition gfCategoryFilter `Sql.and`
  anyTagCondition gfAnyTagFilter `Sql.and`
  allTagsCondition gfAllTagsFilter `Sql.and`
  titleCondition gfTitleSubstrings `Sql.and`
  bodyCondition gfBodySubstrings `Sql.and`
  substringsAnywhereCondition gfSubstringsAnywhere

dateCondition :: Maybe (N.NonEmpty IListNews.NewsDateRange) -> Sql.Builder
dateCondition =
  maybe mempty $ foldr (Sql.or . sqlWithinDateRange "\"date\"") mempty

sqlWithinDateRange :: Sql.Builder -> IListNews.NewsDateRange -> Sql.Builder
sqlWithinDateRange expr dateRange =
  case dateRange of
    IListNews.NewsSinceUntil from to
      | from == to -> expr `Sql.equal` Sql.param from
      | otherwise -> expr `Sql.between` (Sql.param from, Sql.param to)
    IListNews.NewsSince day -> expr `Sql.greaterOrEqual` Sql.param day
    IListNews.NewsUntil day -> expr `Sql.lessOrEqual` Sql.param day

authorCondition :: IListNews.GatewayAuthorFilter -> Sql.Builder
authorCondition IListNews.GatewayAuthorFilter {..} =
  maybe mempty idCondition gfAuthorIds `Sql.or`
  maybe mempty authorSubstringsCondition gfAuthorNameSubstrings
  where
    idCondition =
      ("authors.author_id =" <>) .
      Sql.any . Sql.param . map getAuthorId . toList

authorSubstringsCondition :: Set.HashSet T.Text -> Sql.Builder
authorSubstringsCondition =
  ("first_to_last_name ilike" <>) .
  Sql.any . stringsToLikeSubstringPatternsParameter

categoryCondition :: IListNews.GatewayCategoryFilter -> Sql.Builder
categoryCondition IListNews.GatewayCategoryFilter {..} =
  maybe mempty idCondition gfCategoryIds `Sql.or`
  maybe mempty categorySubstringsCondition gfCategoryNameSubstrings
  where
    idCondition =
      ("category_id in (select * from descendants_of_categories_with_ids(" <>) .
      (<> "))") . Sql.param . map getCategoryId . toList

categorySubstringsCondition :: Set.HashSet T.Text -> Sql.Builder
categorySubstringsCondition =
  ("category_id in (select * from descendants_of_categories_named_like(" <>) .
  (<> "))") . stringsToLikeSubstringPatternsParameter

anyTagCondition :: IListNews.GatewayAnyTagFilter -> Sql.Builder
anyTagCondition IListNews.GatewayAnyTagFilter {..} =
  maybe mempty idCondition gfTagIdsToMatchAnyTag `Sql.or`
  maybe mempty anyTagSubstringsCondition gfTagNameSubstringsToMatchAnyTag
  where
    idCondition =
      ("tags.tag_id =" <>) . Sql.any . Sql.param . map getTagId . toList

anyTagSubstringsCondition :: Set.HashSet T.Text -> Sql.Builder
anyTagSubstringsCondition =
  ("tags.name ilike" <>) . Sql.any . stringsToLikeSubstringPatternsParameter

allTagsCondition :: IListNews.GatewayAllTagsFilter -> Sql.Builder
allTagsCondition IListNews.GatewayAllTagsFilter {..} =
  maybe mempty idCondition gfTagIdsAllRequiredToMatch `Sql.or`
  maybe mempty nameCondition gfTagNameSubstringsAllRequiredToMatch
  where
    idCondition =
      ("news.news_version_id in (select * from news_version_ids_connected_with_all_tags_with_ids(" <>) .
      (<> "))") . Sql.param . map getTagId . toList
    nameCondition =
      ("news.news_version_id in (select * from news_version_ids_connected_with_all_tags_like(" <>) .
      (<> "))") . stringsToLikeSubstringPatternsParameter

titleCondition :: Maybe (Set.HashSet T.Text) -> Sql.Builder
titleCondition =
  maybe mempty $
  ("news_versions.title ilike" <>) .
  Sql.any . stringsToLikeSubstringPatternsParameter

bodyCondition :: Maybe (Set.HashSet T.Text) -> Sql.Builder
bodyCondition =
  maybe mempty $
  ("news_versions.body ilike" <>) .
  Sql.any . stringsToLikeSubstringPatternsParameter

substringsAnywhereCondition :: Maybe (Set.HashSet T.Text) -> Sql.Builder
substringsAnywhereCondition Nothing = mempty
substringsAnywhereCondition (Just substrings) =
  Sql.bracket $
  titleCondition (Just substrings) `Sql.or` authorSubstringsCondition substrings `Sql.or`
  categorySubstringsCondition substrings `Sql.or`
  anyTagSubstringsCondition substrings `Sql.or`
  bodyCondition (Just substrings)

stringsToLikeSubstringPatternsParameter :: Foldable t => t T.Text -> Sql.Builder
stringsToLikeSubstringPatternsParameter =
  Sql.param . map Sql.substringLikePattern . toList
