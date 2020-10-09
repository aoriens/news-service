module Core.Interactor.GetTagsSpec
  ( spec
  ) where

import Control.Monad
import Core.Interactor.GetTags
import Core.Pagination
import Core.Pagination.Test
import Core.Tag
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should return tags from the gateway" $ do
      let expectedTags = [stubTag {tagId = TagId 9}]
          h = defaultHandle {hGetTags = const $ pure expectedTags}
      tags <- run h noPageQuery
      tags `shouldBe` expectedTags
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            defaultHandle
              { hGetTags = \pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ run h pageSpecQuery

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetTags = const $ pure []
    , hPageSpecParserHandle = PageSpecParserHandle . const $ Right defaultPage
    }

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

defaultPage :: PageSpec
defaultPage = PageSpec (PageOffset 0) (PageLimit 0)

stubTag :: Tag
stubTag = Tag {tagId = TagId 1, tagName = "q"}
