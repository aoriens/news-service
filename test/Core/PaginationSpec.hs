module Core.PaginationSpec
  ( spec
  ) where

import Core.Pagination
import Test.Hspec

spec :: Spec
spec =
  describe "fromPageQuery" $ do
    it "should return max limit if no limit specified" $ do
      let maxLimit = PageLimit 10
          r = fromPageQuery maxLimit noPageQuery
      pageLimit r `shouldBe` maxLimit
    it "should return max limit if specified limit is greater" $ do
      let maxLimit = PageLimit 10
          limit = PageLimit 11
          r = fromPageQuery maxLimit noPageQuery {pageQueryLimit = Just limit}
      pageLimit r `shouldBe` maxLimit
    it "should return the specified page limit if less than max limit" $ do
      let maxLimit = PageLimit 10
          limit = PageLimit 9
          r = fromPageQuery maxLimit noPageQuery {pageQueryLimit = Just limit}
      pageLimit r `shouldBe` limit
    it "should return offset if specified" $ do
      let offset = PageOffset 5
          r =
            fromPageQuery
              defaultMaxLimit
              noPageQuery {pageQueryOffset = Just offset}
      pageOffset r `shouldBe` offset
    it "should return zero offset if no offset specified" $ do
      let r =
            fromPageQuery
              defaultMaxLimit
              noPageQuery {pageQueryOffset = Nothing}
      pageOffset r `shouldBe` PageOffset 0

noPageQuery :: PageQuery
noPageQuery = PageQuery Nothing Nothing

defaultMaxLimit :: PageLimit
defaultMaxLimit = PageLimit 10
