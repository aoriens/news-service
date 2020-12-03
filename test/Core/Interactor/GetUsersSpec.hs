module Core.Interactor.GetUsersSpec
  ( spec
  ) where

import Control.Monad
import Core.Interactor.GetUsers
import Core.Pagination
import Core.Pagination.Test
import Core.Stubs
import Core.User
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    it "should return users from the gateway" $ do
      let expectedUsers = [stubUser {userId = UserId 9}]
          h = defaultHandle {hGetUsers = const $ pure expectedUsers}
      users <- run h noPageQuery
      users `shouldBe` expectedUsers
    itShouldWorkWithPageSpecParserCorrectly $ \hPageSpecParserHandle pageSpecQuery onSuccess -> do
      let h =
            defaultHandle
              { hGetUsers = \pageQuery -> onSuccess pageQuery >> pure []
              , hPageSpecParserHandle
              }
      void $ run h pageSpecQuery

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetUsers = const $ pure []
    , hPageSpecParserHandle = PageSpecParserHandle . const $ Right defaultPage
    }

noPageQuery :: PageSpecQuery
noPageQuery = PageSpecQuery Nothing Nothing

defaultPage :: PageSpec
defaultPage = PageSpec (PageOffset 0) (PageLimit 0)
