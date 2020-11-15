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
          h = stubHandle {hGetNews = \_ _ -> pure expectedNews}
      results <- I.getNews h emptyNewsFilter noPageQuery
      results `shouldBe` expectedNews
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            stubHandle
              { hGetNews = \_ pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ I.getNews h emptyNewsFilter pageSpecQuery
    it "should pass NewsFilter data to hGetNews" $ do
      ref <- newIORef []
      let newsFilter =
            I.NewsFilter
              { nfDateRanges =
                  N.nonEmpty
                    [ I.NewsSinceUntil
                        (ModifiedJulianDay 1)
                        (ModifiedJulianDay 2)
                    ]
              , nfAuthorIds = Just $ Set.fromList [AuthorId 1]
              , nfAuthorNameSubstrings = Just $ Set.fromList ["a"]
              , nfCategoryIds = Just $ Set.fromList [CategoryId 1]
              , nfCategoryNameSubstrings = Just $ Set.fromList ["c"]
              , nfTagIdsToMatchAnyTag = Just $ Set.fromList [TagId 1]
              , nfTagNameSubstringsToMatchAnyTag = Just $ Set.fromList ["t"]
              , nfTagIdsAllRequiredToMatch = Just $ Set.fromList [TagId 2]
              , nfTagNameSubstringsAllRequiredToMatch =
                  Just $ Set.fromList ["T"]
              , nfTitleSubstrings = Just $ Set.fromList ["title"]
              , nfBodySubstrings = Just $ Set.fromList ["body"]
              , nfSubstringsAnywhere = Just $ Set.fromList ["q"]
              }
          h = stubHandle {hGetNews = \f _ -> modifyIORef' ref (f :) >> pure []}
      _ <- I.getNews h newsFilter noPageQuery
      passedFilters <- readIORef ref
      fmap gnfDateRanges passedFilters `shouldBe` [I.nfDateRanges newsFilter]
      fmap (gnfAuthorIds . gnfAuthorFilter) passedFilters `shouldBe`
        [I.nfAuthorIds newsFilter]
      fmap (gnfAuthorNameSubstrings . gnfAuthorFilter) passedFilters `shouldBe`
        [I.nfAuthorNameSubstrings newsFilter]
      fmap (gnfCategoryIds . gnfCategoryFilter) passedFilters `shouldBe`
        [I.nfCategoryIds newsFilter]
      fmap (gnfCategoryNameSubstrings . gnfCategoryFilter) passedFilters `shouldBe`
        [I.nfCategoryNameSubstrings newsFilter]
      fmap (gnfTagIdsToMatchAnyTag . gnfAnyTagFilter) passedFilters `shouldBe`
        [I.nfTagIdsToMatchAnyTag newsFilter]
      fmap (gnfTagNameSubstringsToMatchAnyTag . gnfAnyTagFilter) passedFilters `shouldBe`
        [I.nfTagNameSubstringsToMatchAnyTag newsFilter]
      fmap (gnfTagIdsAllRequiredToMatch . gnfAllTagsFilter) passedFilters `shouldBe`
        [I.nfTagIdsAllRequiredToMatch newsFilter]
      fmap
        (gnfTagNameSubstringsAllRequiredToMatch . gnfAllTagsFilter)
        passedFilters `shouldBe`
        [I.nfTagNameSubstringsAllRequiredToMatch newsFilter]
      fmap gnfTitleSubstrings passedFilters `shouldBe`
        [I.nfTitleSubstrings newsFilter]
      fmap gnfBodySubstrings passedFilters `shouldBe`
        [I.nfBodySubstrings newsFilter]
      fmap gnfSubstringsAnywhere passedFilters `shouldBe`
        [I.nfSubstringsAnywhere newsFilter]
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
            I.NewsFilter
              { nfDateRanges = Nothing
              , nfAuthorIds = Nothing
              , nfAuthorNameSubstrings =
                  Just $ Set.insert "" expectedAuthorNameSubstrings
              , nfCategoryIds = Nothing
              , nfCategoryNameSubstrings =
                  Just $ Set.insert "" expectedCategoryNameSubstrings
              , nfTagIdsToMatchAnyTag = Nothing
              , nfTagNameSubstringsToMatchAnyTag =
                  Just $ Set.insert "" expectedAnyTagNameSubstrings
              , nfTagIdsAllRequiredToMatch = Nothing
              , nfTagNameSubstringsAllRequiredToMatch =
                  Just $ Set.insert "" expectedAllTagsNameSubstrings
              , nfTitleSubstrings = Just $ Set.insert "" expectedTitleSubstrings
              , nfBodySubstrings = Just $ Set.insert "" expectedBodySubstrings
              , nfSubstringsAnywhere =
                  Just $ Set.insert "" expectedSubstringsAnywhere
              }
          h = stubHandle {hGetNews = \f _ -> modifyIORef' ref (f :) >> pure []}
      _ <- I.getNews h newsFilter noPageQuery
      passedFilters <- readIORef ref
      fmap (gnfAuthorNameSubstrings . gnfAuthorFilter) passedFilters `shouldBe`
        [Just expectedAuthorNameSubstrings]
      fmap (gnfCategoryNameSubstrings . gnfCategoryFilter) passedFilters `shouldBe`
        [Just expectedCategoryNameSubstrings]
      fmap (gnfTagNameSubstringsToMatchAnyTag . gnfAnyTagFilter) passedFilters `shouldBe`
        [Just expectedAnyTagNameSubstrings]
      fmap
        (gnfTagNameSubstringsAllRequiredToMatch . gnfAllTagsFilter)
        passedFilters `shouldBe`
        [Just expectedAllTagsNameSubstrings]
      fmap gnfTitleSubstrings passedFilters `shouldBe`
        [Just expectedTitleSubstrings]
      fmap gnfBodySubstrings passedFilters `shouldBe`
        [Just expectedBodySubstrings]
      fmap gnfSubstringsAnywhere passedFilters `shouldBe`
        [Just expectedSubstringsAnywhere]
    it
      "should pass Nothing as a substrings filter to hGetNews if the filter contains an empty string only" $ do
      ref <- newIORef []
      let newsFilter =
            I.NewsFilter
              { nfDateRanges = Nothing
              , nfAuthorIds = Nothing
              , nfAuthorNameSubstrings = Just $ Set.singleton ""
              , nfCategoryIds = Nothing
              , nfCategoryNameSubstrings = Just $ Set.singleton ""
              , nfTagIdsToMatchAnyTag = Nothing
              , nfTagNameSubstringsToMatchAnyTag = Just $ Set.singleton ""
              , nfTagIdsAllRequiredToMatch = Nothing
              , nfTagNameSubstringsAllRequiredToMatch = Just $ Set.singleton ""
              , nfTitleSubstrings = Just $ Set.singleton ""
              , nfBodySubstrings = Just $ Set.singleton ""
              , nfSubstringsAnywhere = Just $ Set.singleton ""
              }
          h = stubHandle {hGetNews = \f _ -> modifyIORef' ref (f :) >> pure []}
      _ <- I.getNews h newsFilter noPageQuery
      passedFilters <- readIORef ref
      fmap (gnfAuthorNameSubstrings . gnfAuthorFilter) passedFilters `shouldBe`
        [Nothing]
      fmap (gnfCategoryNameSubstrings . gnfCategoryFilter) passedFilters `shouldBe`
        [Nothing]
      fmap (gnfTagNameSubstringsToMatchAnyTag . gnfAnyTagFilter) passedFilters `shouldBe`
        [Nothing]
      fmap
        (gnfTagNameSubstringsAllRequiredToMatch . gnfAllTagsFilter)
        passedFilters `shouldBe`
        [Nothing]
      fmap gnfTitleSubstrings passedFilters `shouldBe` [Nothing]
      fmap gnfBodySubstrings passedFilters `shouldBe` [Nothing]
      fmap gnfSubstringsAnywhere passedFilters `shouldBe` [Nothing]

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

stubHandle :: I.Handle IO
stubHandle =
  I.Handle
    { hPageSpecParserHandle =
        PageSpecParserHandle . const . Right $
        PageSpec (PageOffset 0) (PageLimit 0)
    , hGetNews = \_ _ -> pure []
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
