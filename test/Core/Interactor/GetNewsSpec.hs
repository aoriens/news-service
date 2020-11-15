module Core.Interactor.GetNewsSpec
  ( spec
  ) where

import Control.Monad
import Core.Author
import Core.Category
import Core.Interactor.GetNews as I
import Core.News
import Core.Pagination
import Core.Pagination.Test
import Core.Tag
import Core.User
import qualified Data.HashSet as Set
import Data.IORef
import qualified Data.List.NonEmpty as N
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "getNews" $ do
    it "should pass data through from the gateway" $ do
      let expectedNews =
            [ stubNews
                { newsId = NewsId 1
                , newsVersion =
                    stubNewsVersion
                      {nvId = NewsVersionId 1, nvTitle = "A", nvText = "Text"}
                }
            , stubNews
                { newsId = NewsId 2
                , newsVersion =
                    stubNewsVersion
                      {nvId = NewsVersionId 2, nvTitle = "B", nvText = "Text2"}
                }
            ]
          h = stubHandle {hGetNews = \_ _ _ -> pure expectedNews}
      results <- I.getNews h emptyFilter defaultSortOptions noPageQuery
      results `shouldBe` expectedNews
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            stubHandle
              { hGetNews = \_ _ pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ I.getNews h emptyFilter defaultSortOptions pageSpecQuery
    it "should pass Filter data to hGetNews" $ do
      ref <- newIORef []
      let newsFilter =
            I.Filter
              { fDateRanges =
                  N.nonEmpty
                    [ I.NewsSinceUntil
                        (ModifiedJulianDay 1)
                        (ModifiedJulianDay 2)
                    ]
              , fAuthorIds = Just $ Set.fromList [AuthorId 1]
              , fAuthorNameSubstrings = Just $ Set.fromList ["a"]
              , fCategoryIds = Just $ Set.fromList [CategoryId 1]
              , fCategoryNameSubstrings = Just $ Set.fromList ["c"]
              , fTagIdsToMatchAnyTag = Just $ Set.fromList [TagId 1]
              , fTagNameSubstringsToMatchAnyTag = Just $ Set.fromList ["t"]
              , fTagIdsAllRequiredToMatch = Just $ Set.fromList [TagId 2]
              , fTagNameSubstringsAllRequiredToMatch = Just $ Set.fromList ["T"]
              , fTitleSubstrings = Just $ Set.fromList ["title"]
              , fBodySubstrings = Just $ Set.fromList ["body"]
              , fSubstringsAnywhere = Just $ Set.fromList ["q"]
              }
          h =
            stubHandle {hGetNews = \f _ _ -> modifyIORef' ref (f :) >> pure []}
      _ <- I.getNews h newsFilter defaultSortOptions noPageQuery
      passedFilters <- readIORef ref
      fmap gfDateRanges passedFilters `shouldBe` [I.fDateRanges newsFilter]
      fmap (gfAuthorIds . gfAuthorFilter) passedFilters `shouldBe`
        [I.fAuthorIds newsFilter]
      fmap (gfAuthorNameSubstrings . gfAuthorFilter) passedFilters `shouldBe`
        [I.fAuthorNameSubstrings newsFilter]
      fmap (gfCategoryIds . gfCategoryFilter) passedFilters `shouldBe`
        [I.fCategoryIds newsFilter]
      fmap (gfCategoryNameSubstrings . gfCategoryFilter) passedFilters `shouldBe`
        [I.fCategoryNameSubstrings newsFilter]
      fmap (gfTagIdsToMatchAnyTag . gfAnyTagFilter) passedFilters `shouldBe`
        [I.fTagIdsToMatchAnyTag newsFilter]
      fmap (gfTagNameSubstringsToMatchAnyTag . gfAnyTagFilter) passedFilters `shouldBe`
        [I.fTagNameSubstringsToMatchAnyTag newsFilter]
      fmap (gfTagIdsAllRequiredToMatch . gfAllTagsFilter) passedFilters `shouldBe`
        [I.fTagIdsAllRequiredToMatch newsFilter]
      fmap
        (gfTagNameSubstringsAllRequiredToMatch . gfAllTagsFilter)
        passedFilters `shouldBe`
        [I.fTagNameSubstringsAllRequiredToMatch newsFilter]
      fmap gfTitleSubstrings passedFilters `shouldBe`
        [I.fTitleSubstrings newsFilter]
      fmap gfBodySubstrings passedFilters `shouldBe`
        [I.fBodySubstrings newsFilter]
      fmap gfSubstringsAnywhere passedFilters `shouldBe`
        [I.fSubstringsAnywhere newsFilter]
    it "should not pass empty substring filters to hGetNews to optimize search" $ do
      ref <- newIORef []
      let expectedAuthorNameSubstrings = Set.fromList ["a"]
          expectedCategoryNameSubstrings = Set.fromList ["c"]
          expectedAnyTagNameSubstrings = Set.fromList ["t"]
          expectedAllTagsNameSubstrings = Set.fromList ["T"]
          expectedTitleSubstrings = Set.fromList ["title"]
          expectedBodySubstrings = Set.fromList ["body"]
          expectedSubstringsAnywhere = Set.fromList ["q"]
          newsFilter =
            I.Filter
              { fDateRanges = Nothing
              , fAuthorIds = Nothing
              , fAuthorNameSubstrings =
                  Just $ Set.insert "" expectedAuthorNameSubstrings
              , fCategoryIds = Nothing
              , fCategoryNameSubstrings =
                  Just $ Set.insert "" expectedCategoryNameSubstrings
              , fTagIdsToMatchAnyTag = Nothing
              , fTagNameSubstringsToMatchAnyTag =
                  Just $ Set.insert "" expectedAnyTagNameSubstrings
              , fTagIdsAllRequiredToMatch = Nothing
              , fTagNameSubstringsAllRequiredToMatch =
                  Just $ Set.insert "" expectedAllTagsNameSubstrings
              , fTitleSubstrings = Just $ Set.insert "" expectedTitleSubstrings
              , fBodySubstrings = Just $ Set.insert "" expectedBodySubstrings
              , fSubstringsAnywhere =
                  Just $ Set.insert "" expectedSubstringsAnywhere
              }
          h =
            stubHandle {hGetNews = \f _ _ -> modifyIORef' ref (f :) >> pure []}
      _ <- I.getNews h newsFilter defaultSortOptions noPageQuery
      passedFilters <- readIORef ref
      fmap (gfAuthorNameSubstrings . gfAuthorFilter) passedFilters `shouldBe`
        [Just expectedAuthorNameSubstrings]
      fmap (gfCategoryNameSubstrings . gfCategoryFilter) passedFilters `shouldBe`
        [Just expectedCategoryNameSubstrings]
      fmap (gfTagNameSubstringsToMatchAnyTag . gfAnyTagFilter) passedFilters `shouldBe`
        [Just expectedAnyTagNameSubstrings]
      fmap
        (gfTagNameSubstringsAllRequiredToMatch . gfAllTagsFilter)
        passedFilters `shouldBe`
        [Just expectedAllTagsNameSubstrings]
      fmap gfTitleSubstrings passedFilters `shouldBe`
        [Just expectedTitleSubstrings]
      fmap gfBodySubstrings passedFilters `shouldBe`
        [Just expectedBodySubstrings]
      fmap gfSubstringsAnywhere passedFilters `shouldBe`
        [Just expectedSubstringsAnywhere]
    it
      "should pass Nothing as a substrings filter to hGetNews if the filter contains an empty string only" $ do
      ref <- newIORef []
      let newsFilter =
            I.Filter
              { fDateRanges = Nothing
              , fAuthorIds = Nothing
              , fAuthorNameSubstrings = Just $ Set.singleton ""
              , fCategoryIds = Nothing
              , fCategoryNameSubstrings = Just $ Set.singleton ""
              , fTagIdsToMatchAnyTag = Nothing
              , fTagNameSubstringsToMatchAnyTag = Just $ Set.singleton ""
              , fTagIdsAllRequiredToMatch = Nothing
              , fTagNameSubstringsAllRequiredToMatch = Just $ Set.singleton ""
              , fTitleSubstrings = Just $ Set.singleton ""
              , fBodySubstrings = Just $ Set.singleton ""
              , fSubstringsAnywhere = Just $ Set.singleton ""
              }
          h =
            stubHandle {hGetNews = \f _ _ -> modifyIORef' ref (f :) >> pure []}
      _ <- I.getNews h newsFilter defaultSortOptions noPageQuery
      passedFilters <- readIORef ref
      fmap (gfAuthorNameSubstrings . gfAuthorFilter) passedFilters `shouldBe`
        [Nothing]
      fmap (gfCategoryNameSubstrings . gfCategoryFilter) passedFilters `shouldBe`
        [Nothing]
      fmap (gfTagNameSubstringsToMatchAnyTag . gfAnyTagFilter) passedFilters `shouldBe`
        [Nothing]
      fmap
        (gfTagNameSubstringsAllRequiredToMatch . gfAllTagsFilter)
        passedFilters `shouldBe`
        [Nothing]
      fmap gfTitleSubstrings passedFilters `shouldBe` [Nothing]
      fmap gfBodySubstrings passedFilters `shouldBe` [Nothing]
      fmap gfSubstringsAnywhere passedFilters `shouldBe` [Nothing]
    it "should pass SortOptions to hGetNews" $ do
      ref <- newIORef []
      let sortOptions =
            I.SortOptions {sortReverse = True, sortKey = I.SortKeyAuthorName}
          h =
            stubHandle
              {hGetNews = \_ opts _ -> modifyIORef' ref (opts :) >> pure []}
      _ <- I.getNews h I.emptyFilter sortOptions noPageQuery
      readIORef ref `shouldReturn` [sortOptions]

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

stubHandle :: I.Handle IO
stubHandle =
  I.Handle
    { hPageSpecParserHandle =
        PageSpecParserHandle . const . Right $
        PageSpec (PageOffset 0) (PageLimit 0)
    , hGetNews = \_ _ _ -> pure []
    }

stubNews :: News
stubNews =
  News
    { newsId = NewsId (-1)
    , newsDate = ModifiedJulianDay (-1)
    , newsVersion = stubNewsVersion
    }

stubNewsVersion :: NewsVersion
stubNewsVersion =
  NewsVersion
    { nvId = NewsVersionId 0
    , nvTitle = ""
    , nvText = ""
    , nvAuthor =
        Author
          { authorId = AuthorId 1
          , authorDescription = ""
          , authorUser =
              User
                { userId = UserId 1
                , userLastName = ""
                , userFirstName = Nothing
                , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
                , userAvatarId = Nothing
                , userIsAdmin = False
                }
          }
    , nvCategory =
        Category
          { categoryId = CategoryId 1
          , categoryName = "q"
          , categoryParent = Nothing
          }
    , nvMainPhotoId = Nothing
    , nvAdditionalPhotoIds = Set.empty
    , nvTags = Set.singleton Tag {tagId = TagId 1, tagName = "q"}
    }
