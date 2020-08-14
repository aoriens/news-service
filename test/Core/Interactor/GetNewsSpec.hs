{-# LANGUAGE RecordWildCards #-}

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
          hMaxPageLimit = PageLimit maxBound
          hGetNews = const (pure stubResults)
      results <- I.getNews I.Handle {..} noPageQuery
      results `shouldBe` stubResults
    it "should pass the given max page limit to the gateway" $ do
      passedLimit <- newIORef (PageLimit 0)
      let hMaxPageLimit = PageLimit 10
          hGetNews = \Page {..} -> writeIORef passedLimit pageLimit >> pure []
      _ <- I.getNews I.Handle {..} noPageQuery
      readIORef passedLimit `shouldReturn` hMaxPageLimit
    it "should pass the page offset to the gateway" $ do
      passedOffset <- newIORef (PageOffset 0)
      let hMaxPageLimit = PageLimit 1
          offset = PageOffset 3
          hGetNews = \Page {..} -> writeIORef passedOffset pageOffset >> pure []
      _ <- I.getNews I.Handle {..} noPageQuery {pageQueryOffset = Just offset}
      readIORef passedOffset `shouldReturn` offset
    it "should pass offset=0 to the gateway if no offset specified" $ do
      passedOffset <- newIORef (PageOffset 0)
      let hMaxPageLimit = PageLimit 1
          hGetNews = \Page {..} -> writeIORef passedOffset pageOffset >> pure []
      _ <- I.getNews I.Handle {..} noPageQuery
      readIORef passedOffset `shouldReturn` PageOffset 0
    it
      "should pass the specified page limit to the gateway if less than max limit" $ do
      passedLimit <- newIORef (PageLimit 0)
      let hMaxPageLimit = PageLimit 10
          limit = PageLimit 9
          hGetNews = \Page {..} -> writeIORef passedLimit pageLimit >> pure []
      _ <- I.getNews I.Handle {..} noPageQuery {pageQueryLimit = Just limit}
      readIORef passedLimit `shouldReturn` limit
    it "should pass the default limit if no one specified" $ do
      passedLimit <- newIORef (PageLimit 0)
      let hMaxPageLimit = PageLimit 10
          hGetNews = \Page {..} -> writeIORef passedLimit pageLimit >> pure []
      _ <- I.getNews I.Handle {..} noPageQuery {pageQueryLimit = Nothing}
      readIORef passedLimit `shouldReturn` hMaxPageLimit

noPageQuery :: PageQuery
noPageQuery = PageQuery Nothing Nothing
