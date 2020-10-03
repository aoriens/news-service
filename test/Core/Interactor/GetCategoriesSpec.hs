module Core.Interactor.GetCategoriesSpec
  ( spec
  ) where

import Core.Category
import Core.Interactor.GetCategories as IGetCategories
import Core.Pagination
import Data.IORef
import Test.Hspec

spec :: Spec
spec =
  describe "getNews" $ do
    it "should pass data through from the gateway" $ do
      let stubResults = [stubCategory]
          h = stubHandle {hGetCategories = const (pure stubResults)}
      results <- IGetCategories.run h noPageQuery
      results `shouldBe` stubResults
    it "should pass the page spec to the gateway" $ do
      passedPage <- newIORef Nothing
      let page = PageSpec (PageOffset 1) (PageLimit 2)
          h =
            stubHandle
              { hPageSpecParserHandle =
                  PageSpecParserHandle . const $ Right page
              , hGetCategories = \p -> writeIORef passedPage (Just p) >> pure []
              }
      _ <- IGetCategories.run h noPageQuery
      readIORef passedPage `shouldReturn` Just page
    it "should pass the page query spec to the page spec parser" $ do
      passedPage <- newIORef Nothing
      let offset = 4
          limit = 3
          pageQuery = PageSpecQuery (Just offset) (Just limit)
          expectedPage = PageSpec (PageOffset offset) (PageLimit limit)
          unexpectedPage = PageSpec (PageOffset 0) (PageLimit 0)
          h =
            stubHandle
              { hPageSpecParserHandle =
                  PageSpecParserHandle $ \p ->
                    Right $
                    if p == pageQuery
                      then expectedPage
                      else unexpectedPage
              , hGetCategories = \p -> writeIORef passedPage (Just p) >> pure []
              }
      _ <- IGetCategories.run h pageQuery
      readIORef passedPage `shouldReturn` Just expectedPage

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

stubCategory :: Category
stubCategory =
  Category
    { categoryName = "haskell"
    , categoryId = CategoryId 1
    , categoryParent =
        Just
          Category
            { categoryName = "programming"
            , categoryId = CategoryId 2
            , categoryParent = Nothing
            }
    }

stubHandle :: Handle IO
stubHandle =
  Handle
    { hGetCategories = \_ -> pure []
    , hPageSpecParserHandle =
        PageSpecParserHandle . const . Right $
        PageSpec (PageOffset 0) (PageLimit 0)
    }
