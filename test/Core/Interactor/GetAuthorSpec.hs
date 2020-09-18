module Core.Interactor.GetAuthorSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import Core.Interactor.GetAuthor
import Core.User
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "run" $ do
    itShouldRequireAdminPermission $ \credentials authHandle onSuccess -> do
      let h =
            defaultHandle
              { hGetAuthor = \_ -> onSuccess >> pure (Just stubAuthor)
              , hAuthHandle = authHandle
              }
      void $ run h credentials stubAuthorId
    it "should return gateway output if the actor is admin" $ do
      let expectedAuthor = Just stubAuthor {authorId = AuthorId 9}
          h =
            defaultHandle
              { hGetAuthor = const $ pure expectedAuthor
              , hAuthHandle = stubAuthHandleReturningAdminUser
              }
      author <- run h noCredentials stubAuthorId
      author `shouldBe` expectedAuthor
    it "should pass author id to the gateway" $ do
      passedAuthorId <- newIORef undefined
      let expectedAuthorId = AuthorId 8
          h =
            defaultHandle
              {hGetAuthor = \i -> writeIORef passedAuthorId i >> pure Nothing}
      _ <- run h noCredentials expectedAuthorId
      readIORef passedAuthorId `shouldReturn` expectedAuthorId

defaultHandle :: Handle IO
defaultHandle =
  Handle
    { hGetAuthor = const $ pure Nothing
    , hAuthHandle = stubAuthHandleReturningAdminUser
    }

stubAuthorId :: AuthorId
stubAuthorId = AuthorId 1

stubAuthor :: Author
stubAuthor =
  Author
    { authorId = AuthorId 1
    , authorDescription = ""
    , authorUser =
        User
          { userId = UserId 1
          , userFirstName = Nothing
          , userLastName = ""
          , userAvatarId = Nothing
          , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
          , userIsAdmin = False
          }
    }
