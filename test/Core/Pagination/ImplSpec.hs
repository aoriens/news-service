module Core.Pagination.ImplSpec
  ( spec
  ) where

import Core.Pagination
import qualified Core.Pagination.Impl as Impl
import Test.Hspec

spec :: Spec
spec =
  describe "pageFromPageQuery" $ do
    it "should return max limit if no limit specified" $ do
      let maxLimit = PageLimit 10
          config = Impl.new maxLimit
          r = pageFromPageQuery config noPageQuery
      pageLimit <$> r `shouldBe` Just maxLimit
    it "should return max limit if specified limit is greater" $ do
      let maxLimit = PageLimit 10
          config = Impl.new maxLimit
          limit = 11
          r = pageFromPageQuery config noPageQuery {pageQueryLimit = Just limit}
      pageLimit <$> r `shouldBe` Just maxLimit
    it "should return the specified page limit if less than max limit" $ do
      let config = Impl.new $ PageLimit 10
          limit = 9
          r = pageFromPageQuery config noPageQuery {pageQueryLimit = Just limit}
      pageLimit <$> r `shouldBe` Just (PageLimit limit)
    it "should return offset if specified" $ do
      let offset = 5
          r =
            pageFromPageQuery
              defaultHandle
              noPageQuery {pageQueryOffset = Just offset}
      pageOffset <$> r `shouldBe` Just (PageOffset offset)
    it "should return zero offset if no offset specified" $ do
      let r =
            pageFromPageQuery
              defaultHandle
              noPageQuery {pageQueryOffset = Nothing}
      pageOffset <$> r `shouldBe` Just (PageOffset 0)
    it "should return Nothing if offset is negative" $ do
      let r =
            pageFromPageQuery
              defaultHandle
              noPageQuery {pageQueryOffset = Just (-1)}
      r `shouldBe` Nothing
    it "should return Nothing if limit is negative" $ do
      let r =
            pageFromPageQuery
              defaultHandle
              noPageQuery {pageQueryLimit = Just (-1)}
      r `shouldBe` Nothing

noPageQuery :: PageQuery
noPageQuery = PageQuery Nothing Nothing

defaultHandle :: PagerHandle
defaultHandle = Impl.new $ PageLimit 10
