module Core.Interactor.GetAuthorsSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
import Core.Interactor.GetAuthors
import Core.Pagination
import Core.Pagination.Test
import Core.Stubs
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldAuthorizeBeforeOperation AdminPermission $ \authUser authorizationHandle onSuccess -> do
      let h =
            defaultHandle
              { hGetAuthors = \_ -> onSuccess >> pure [stubAuthor]
              , hAuthorizationHandle = authorizationHandle
              }
      void $ run h authUser noPageQuery
    it "should return authors from the gateway, if the actor is admin" $ do
      let expectedAuthors = [stubAuthor {authorId = AuthorId 9}]
          h = defaultHandle {hGetAuthors = const $ pure expectedAuthors}
      authors <- run h someAuthUser noPageQuery
      authors `shouldBe` expectedAuthors
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            defaultHandle
              { hGetAuthors = \pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ run h someAuthUser pageSpecQuery

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetAuthors = const $ pure []
    , hPageSpecParserHandle = PageSpecParserHandle . const $ Right defaultPage
    , hAuthorizationHandle = noOpAuthorizationHandle
    }

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

defaultPage :: PageSpec
defaultPage = PageSpec (PageOffset 0) (PageLimit 0)
