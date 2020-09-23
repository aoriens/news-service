module Core.Interactor.UpdateAuthorSpec
  ( spec
  ) where

import Control.Monad
import Core.Authentication.Test
import Core.Author
import Core.Authorization
import Core.Authorization.Test
import Core.Exception
import Core.Interactor.UpdateAuthor
import Core.User
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec
  {- HLINT ignore spec "Reduce duplication" -}
 =
  describe "run" $ do
    itShouldAuthenticateAndAuthorizeBeforeOperation AdminPermission $ \credentials authHandle authorizationHandle onSuccess -> do
      let aid = AuthorId 1
          description = ""
          h =
            stubHandle
              { hUpdateAuthor =
                  \_ _ -> do
                    onSuccess
                    pure $ Just stubAuthor
              , hAuthHandle = authHandle
              , hAuthorizationHandle = authorizationHandle
              }
      void $ run h credentials aid description
    it
      "should pass authorId and description data to the gateway in a normal case" $ do
      authorIdAndDescription <- newIORef undefined
      let expectedAuthorId = AuthorId 6
          expectedDescription = "q"
          h =
            stubHandle
              { hUpdateAuthor =
                  \aid desc -> do
                    writeIORef authorIdAndDescription (aid, desc)
                    pure $ Just stubAuthor
              }
      _ <- run h noCredentials expectedAuthorId expectedDescription
      readIORef authorIdAndDescription `shouldReturn`
        (expectedAuthorId, expectedDescription)
    it "should return author returned from the gateway if updated successfully" $ do
      let aid = AuthorId 1
          description = "q"
          expectedAuthor = stubAuthor
          h = stubHandle {hUpdateAuthor = \_ _ -> pure $ Just expectedAuthor}
      r <- run h noCredentials aid description
      r `shouldBe` expectedAuthor
    it "should throw EntityNotFoundException if the gateway returned Nothing" $ do
      let aid = AuthorId 1
          description = "q"
          h = stubHandle {hUpdateAuthor = \_ _ -> pure Nothing}
      run h noCredentials aid description `shouldThrow`
        isEntityNotFoundException

stubHandle :: Handle IO
stubHandle =
  Handle
    { hUpdateAuthor = \_ _ -> pure $ Just stubAuthor {authorId = AuthorId 99993}
    , hAuthHandle = noOpAuthenticationHandle
    , hAuthorizationHandle = noOpAuthorizationHandle
    }

stubAuthor :: Author
stubAuthor =
  Author
    { authorId = AuthorId 9
    , authorDescription = ""
    , authorUser =
        User
          { userId = UserId 12
          , userFirstName = Nothing
          , userLastName = ""
          , userAvatarId = Nothing
          , userCreatedAt = UTCTime (ModifiedJulianDay 0) 0
          , userIsAdmin = False
          }
    }
