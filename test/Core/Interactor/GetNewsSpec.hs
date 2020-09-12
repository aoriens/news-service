module Core.Interactor.GetNewsSpec
  ( spec
  ) where

import Core.Interactor.GetNews as I
import Core.Pagination
import Data.IORef
import Data.Time.Calendar
import Test.Hspec

spec :: Spec
spec =
  describe "getNews" $ do
    it "should pass data through from the gateway" $ do
      let stubResults =
            [ I.News
                { I.newsId = 1
                , I.newsTitle = "A"
                , I.newsDate = fromGregorian 2020 01 01
                , I.newsText = "Text"
                }
            , I.News
                { I.newsId = 2
                , I.newsTitle = "B"
                , I.newsDate = fromGregorian 2020 01 02
                , I.newsText = "Text2"
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
    it "should pass the page to the gateway" $ do
      passedPage <- newIORef undefined
      let page = PageSpec (PageOffset 1) (PageLimit 2)
          h =
            I.Handle
              { hPageSpecParserHandle =
                  PageSpecParserHandle . const $ Right page
              , hGetNews = \p -> writeIORef passedPage p >> pure []
              }
      _ <- I.getNews h noPageQuery
      readIORef passedPage `shouldReturn` page
    it "should pass the page query to the pager" $ do
      passedPage <- newIORef undefined
      let offset = 4
          limit = 3
          pageQuery = PageSpecQuery (Just offset) (Just limit)
          expectedPage = PageSpec (PageOffset offset) (PageLimit limit)
          unexpectedPage = PageSpec (PageOffset 0) (PageLimit 0)
          h =
            I.Handle
              { hPageSpecParserHandle =
                  PageSpecParserHandle $ \p ->
                    Right $
                    if p == pageQuery
                      then expectedPage
                      else unexpectedPage
              , hGetNews = \p -> writeIORef passedPage p >> pure []
              }
      _ <- I.getNews h pageQuery
      readIORef passedPage `shouldReturn` expectedPage

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing
