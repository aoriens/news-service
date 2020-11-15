module Database.Logic.News.Filter
  ( whereClauseToFilterNews
  ) where

import Core.Author
import Core.Category
import qualified Core.Interactor.GetNews as IGetNews
import Core.Tag
import Data.Foldable
import qualified Data.HashSet as Set
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Database.Service.SQLBuilder as Sql
import qualified Database.Service.SQLBuilders as Sql

whereClauseToFilterNews :: IGetNews.GatewayNewsFilter -> Sql.Builder
whereClauseToFilterNews IGetNews.GatewayNewsFilter {..} =
  Sql.mapNonEmpty ("where" <>) $
  selectNewsDateCondition gnfDateRanges `Sql.and`
  selectNewsAuthorCondition gnfAuthorFilter `Sql.and`
  selectNewsCategoryCondition gnfCategoryFilter `Sql.and`
  selectNewsAnyTagCondition gnfAnyTagFilter `Sql.and`
  selectNewsAllTagsCondition gnfAllTagsFilter `Sql.and`
  selectNewsTitleCondition gnfTitleSubstrings `Sql.and`
  selectNewsBodyCondition gnfBodySubstrings `Sql.and`
  selectNewsSubstringsAnywhereCondition gnfSubstringsAnywhere

selectNewsDateCondition ::
     Maybe (N.NonEmpty IGetNews.NewsDateRange) -> Sql.Builder
selectNewsDateCondition =
  maybe mempty $ foldr (Sql.or . sqlWithinDateRange "\"date\"") mempty

sqlWithinDateRange :: Sql.Builder -> IGetNews.NewsDateRange -> Sql.Builder
sqlWithinDateRange expr dateRange =
  case dateRange of
    IGetNews.NewsSinceUntil from to
      | from == to -> expr `Sql.equal` Sql.param from
      | otherwise -> expr `Sql.between` (Sql.param from, Sql.param to)
    IGetNews.NewsSince day -> expr `Sql.greaterOrEqual` Sql.param day
    IGetNews.NewsUntil day -> expr `Sql.lessOrEqual` Sql.param day

selectNewsAuthorCondition :: IGetNews.GatewayNewsAuthorFilter -> Sql.Builder
selectNewsAuthorCondition IGetNews.GatewayNewsAuthorFilter {..} =
  maybe mempty idCondition gnfAuthorIds `Sql.or`
  maybe mempty selectNewsAuthorSubstringsCondition gnfAuthorNameSubstrings
  where
    idCondition =
      ("authors.author_id =" <>) .
      Sql.any . Sql.param . map getAuthorId . toList

selectNewsAuthorSubstringsCondition :: Set.HashSet T.Text -> Sql.Builder
selectNewsAuthorSubstringsCondition =
  (fullName <>) .
  ("ilike" <>) . Sql.any . stringsToLikeSubstringPatternsParameter
  where
    fullName =
      "coalesce(users.first_name || ' ' || users.last_name, users.last_name)"

selectNewsCategoryCondition :: IGetNews.GatewayNewsCategoryFilter -> Sql.Builder
selectNewsCategoryCondition IGetNews.GatewayNewsCategoryFilter {..} =
  maybe mempty idCondition gnfCategoryIds `Sql.or`
  maybe mempty selectNewsCategorySubstringsCondition gnfCategoryNameSubstrings
  where
    idCondition =
      ("category_id in (select * from descendants_of_categories_with_ids(" <>) .
      (<> "))") . Sql.param . map getCategoryId . toList

selectNewsCategorySubstringsCondition :: Set.HashSet T.Text -> Sql.Builder
selectNewsCategorySubstringsCondition =
  ("category_id in (select * from descendants_of_categories_named_like(" <>) .
  (<> "))") . stringsToLikeSubstringPatternsParameter

selectNewsAnyTagCondition :: IGetNews.GatewayNewsAnyTagFilter -> Sql.Builder
selectNewsAnyTagCondition IGetNews.GatewayNewsAnyTagFilter {..} =
  maybe mempty idCondition gnfTagIdsToMatchAnyTag `Sql.or`
  maybe
    mempty
    selectNewsAnyTagSubstringsCondition
    gnfTagNameSubstringsToMatchAnyTag
  where
    idCondition =
      ("tags.tag_id =" <>) . Sql.any . Sql.param . map getTagId . toList

selectNewsAnyTagSubstringsCondition :: Set.HashSet T.Text -> Sql.Builder
selectNewsAnyTagSubstringsCondition =
  ("tags.name ilike" <>) . Sql.any . stringsToLikeSubstringPatternsParameter

selectNewsAllTagsCondition :: IGetNews.GatewayNewsAllTagsFilter -> Sql.Builder
selectNewsAllTagsCondition IGetNews.GatewayNewsAllTagsFilter {..} =
  maybe mempty idCondition gnfTagIdsAllRequiredToMatch `Sql.or`
  maybe mempty nameCondition gnfTagNameSubstringsAllRequiredToMatch
  where
    idCondition =
      ("news.news_version_id in (select * from news_version_ids_connected_with_all_tags_with_ids(" <>) .
      (<> "))") . Sql.param . map getTagId . toList
    nameCondition =
      ("news.news_version_id in (select * from news_version_ids_connected_with_all_tags_like(" <>) .
      (<> "))") . stringsToLikeSubstringPatternsParameter

selectNewsTitleCondition :: Maybe (Set.HashSet T.Text) -> Sql.Builder
selectNewsTitleCondition =
  maybe mempty $
  ("news_versions.title ilike" <>) .
  Sql.any . stringsToLikeSubstringPatternsParameter

selectNewsBodyCondition :: Maybe (Set.HashSet T.Text) -> Sql.Builder
selectNewsBodyCondition =
  maybe mempty $
  ("news_versions.body ilike" <>) .
  Sql.any . stringsToLikeSubstringPatternsParameter

selectNewsSubstringsAnywhereCondition ::
     Maybe (Set.HashSet T.Text) -> Sql.Builder
selectNewsSubstringsAnywhereCondition Nothing = mempty
selectNewsSubstringsAnywhereCondition (Just substrings) =
  Sql.bracket $
  selectNewsTitleCondition (Just substrings) `Sql.or`
  selectNewsAuthorSubstringsCondition substrings `Sql.or`
  selectNewsCategorySubstringsCondition substrings `Sql.or`
  selectNewsAnyTagSubstringsCondition substrings `Sql.or`
  selectNewsBodyCondition (Just substrings)

stringsToLikeSubstringPatternsParameter :: Foldable t => t T.Text -> Sql.Builder
stringsToLikeSubstringPatternsParameter =
  Sql.param . map Sql.substringLikePattern . toList
