module Core.Interactor.GetNewsSpec
  ( spec
  ) where

import Control.Monad
import Core.Interactor.GetNews as I
import Core.News
import Core.Pagination
import Core.Pagination.Test
import Data.Time.Calendar
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
                    NewsVersion
                      {nvId = NewsVersionId 1, nvTitle = "A", nvText = "Text"}
                }
            , News
                { newsId = NewsId 2
                , newsDate = fromGregorian 2020 01 02
                , newsVersion =
                    NewsVersion
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
