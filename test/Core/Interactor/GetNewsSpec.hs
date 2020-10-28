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
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "getNews" $ do
    it "should pass data through from the gateway" $ do
      let stubResults =
            [ News
                { newsId = NewsId 1
                , newsDate = fromGregorian 2020 01 01
                , newsVersion =
                    stubNewsVersion
                      {nvId = NewsVersionId 1, nvTitle = "A", nvText = "Text"}
                }
            , News
                { newsId = NewsId 2
                , newsDate = fromGregorian 2020 01 02
                , newsVersion =
                    stubNewsVersion
                      {nvId = NewsVersionId 2, nvTitle = "B", nvText = "Text2"}
                }
            ]
          h =
            I.Handle
              { hPageSpecParserHandle =
                  PageSpecParserHandle . const . Right $
                  PageSpec (PageOffset 0) (PageLimit 0)
              , hGetNews = const (pure stubResults)
              }
      results <- I.getNews h noPageQuery
      results `shouldBe` stubResults
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            I.Handle
              { hGetNews = \pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ I.getNews h pageSpecQuery

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

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
