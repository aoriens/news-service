module Core.Interactor.GetAuthorsSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import Core.Exception
import Core.Interactor.GetAuthors
import Core.Pagination
import Core.Pagination.Test
import Core.Stubs
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should throw isNoPermissionException if the user is not an admin" $ do
      let h = defaultHandle {hGetAuthors = const $ pure []}
      run h someNonAdminUser noPageQuery `shouldThrow` isNoPermissionException
    it "should return authors from the gateway, if the actor is admin" $ do
      let expectedAuthors = [stubAuthor {authorId = AuthorId 9}]
          h = defaultHandle {hGetAuthors = const $ pure expectedAuthors}
      authors <- run h someAdminUser noPageQuery
      authors `shouldBe` expectedAuthors
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            defaultHandle
              { hGetAuthors = \pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ run h someAdminUser pageSpecQuery

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetAuthors = const $ pure []
    , hPageSpecParserHandle = PageSpecParserHandle . const $ Right defaultPage
    }

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

defaultPage :: PageSpec
defaultPage = PageSpec (PageOffset 0) (PageLimit 0)
