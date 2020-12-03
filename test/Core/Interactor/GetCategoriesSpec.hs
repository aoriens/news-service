module Core.Interactor.GetCategoriesSpec
  ( spec
  ) where

import Control.Monad
import Core.Interactor.GetCategories as IGetCategories
import Core.Pagination
import Core.Pagination.Test
import Core.Stubs
import Test.Hspec

spec :: Spec
spec =
  describe "getNews" $ do
    it "should pass data through from the gateway" $ do
      let stubResults = [stubCategory]
          h = stubHandle {hGetCategories = const (pure stubResults)}
      results <- IGetCategories.run h noPageQuery
      results `shouldBe` stubResults
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            stubHandle
              { hGetCategories = \pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ IGetCategories.run h pageSpecQuery

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

stubHandle :: Handle IO
stubHandle =
  Handle
    { hGetCategories = \_ -> pure []
    , hPageSpecParserHandle =
        PageSpecParserHandle . const . Right $
        PageSpec (PageOffset 0) (PageLimit 0)
    }
