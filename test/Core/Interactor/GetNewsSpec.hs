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
              , nfAuthorNames = Just $ Set.fromList ["q"]
              , nfCategoryIds = Just $ Set.fromList [CategoryId 1]
              , nfCategoryNames = Just $ Set.fromList ["c"]
              }
          h = stubHandle {hGetNews = \f _ -> modifyIORef' ref (f :) >> pure []}
      _ <- I.getNews h newsFilter noPageQuery
      passedFilters <- readIORef ref
      fmap gnfDateRanges passedFilters `shouldBe` [I.nfDateRanges newsFilter]
      fmap (gnfAuthorIds . gnfAuthorFilter) passedFilters `shouldBe`
        [I.nfAuthorIds newsFilter]
      fmap (gnfAuthorNames . gnfAuthorFilter) passedFilters `shouldBe`
        [I.nfAuthorNames newsFilter]
      fmap (gnfCategoryIds . gnfCategoryFilter) passedFilters `shouldBe`
        [I.nfCategoryIds newsFilter]
      fmap (gnfCategoryNames . gnfCategoryFilter) passedFilters `shouldBe`
        [I.nfCategoryNames newsFilter]

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
