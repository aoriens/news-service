{-# LANGUAGE RecordWildCards #-}

module Core.Interactor.CreateUserSpec
  ( spec
  ) where

import Control.Monad
import qualified Core.Authentication as Auth
import Core.Image
import qualified Core.Interactor.CreateUser as I
import Core.User
import Data.IORef
import Data.Time
import Test.AsyncExpectation
import Test.Hspec

spec :: Spec
spec = do
  describe "run" $ do
    it "should pass avatar to hCreateUser" $ do
      ref <- newIORef (error "Must have set Just ImageQuery here")
      let h =
            stubHandle
              { I.hCreateUser =
                  \I.CreateUserCommand {..} -> do
                    writeIORef ref cuAvatar
                    pure stubCreateUserResult
              }
          query = stubQuery
      void $ I.run h query
      readIORef ref `shouldReturn` I.qAvatar query
    it "should pass first/last name to hCreateUser and in the result" $ do
      ref <- newIORef (error "Must have set the first and the last name here")
      let h =
            stubHandle
              { I.hCreateUser =
                  \I.CreateUserCommand {..} -> do
                    writeIORef ref (cuFirstName, cuLastName)
                    pure stubCreateUserResult
              }
          query = stubQuery
      (user, _) <- I.run h query
      readIORef ref `shouldReturn` (I.qFirstName query, I.qLastName query)
      (userFirstName user, userLastName user) `shouldBe`
        (I.qFirstName query, I.qLastName query)
    it "should pass time from hGetCurrentTime to hCreateUser and in the result" $ do
      ref <- newIORef (error "Must have written here the current time")
      let expectedTime = UTCTime (ModifiedJulianDay 1) 0
          h =
            stubHandle
              { I.hCreateUser =
                  \I.CreateUserCommand {..} -> do
                    writeIORef ref cuCreatedAt
                    pure stubCreateUserResult
              , I.hGetCurrentTime = pure expectedTime
              }
      (user, _) <- I.run h stubQuery
      readIORef ref `shouldReturn` expectedTime
      userCreatedAt user `shouldBe` expectedTime
    it "should pass the hash from hGenerateToken to hCreateUser" $ do
      ref <- newIORef (error "Must have written a hash here")
      let expectedHash = Auth.SecretTokenHash "1"
          h =
            stubHandle
              { I.hCreateUser =
                  \I.CreateUserCommand {..} -> do
                    writeIORef ref cuTokenHash
                    pure stubCreateUserResult
              , I.hGenerateToken = pure (stubToken, expectedHash)
              }
      void $ I.run h stubQuery
      readIORef ref `shouldReturn` expectedHash
    it "should return credentials with the token from hGenerateToken" $ do
      let expectedToken = Auth.SecretToken "1"
          h =
            stubHandle {I.hGenerateToken = pure (expectedToken, stubTokenHash)}
      (_, Auth.TokenCredentials _ token) <- I.run h stubQuery
      token `shouldBe` expectedToken
    it "should return UserId from hCreateUser" $ do
      let expectedUserId = UserId 1
          h =
            stubHandle
              { I.hCreateUser =
                  const $
                  pure stubCreateUserResult {I.curUserId = expectedUserId}
              }
      (user, _) <- I.run h stubQuery
      userId user `shouldBe` expectedUserId
    it "should return curAvatarId from hCreateUser" $ do
      let expectedImageId = Just $ ImageId 1
          h =
            stubHandle
              { I.hCreateUser =
                  const $
                  pure stubCreateUserResult {I.curAvatarId = expectedImageId}
              }
      (user, _) <- I.run h stubQuery
      userAvatarId user `shouldBe` expectedImageId
    it "should pass isAdmin = False to hCreateUser and in the result" $ do
      ref <- newIORef $ error "Must have been set a Bool here"
      let h =
            stubHandle
              { I.hCreateUser =
                  \I.CreateUserCommand {..} -> do
                    writeIORef ref cuIsAdmin
                    pure stubCreateUserResult
              }
      (user, _) <- I.run h stubQuery
      userIsAdmin user `shouldBe` False
      readIORef ref `shouldReturn` False
    it "should pass avatar, if qAvatar == Just _, to hRejectDisallowedImage" $ do
      let image = stubImage {imageContentType = "t"}
      shouldPassValue image "hRejectDisallowedImage" $ \pass -> do
        let query = stubQuery {I.qAvatar = Just image}
            h = stubHandle {I.hRejectDisallowedImage = pass}
        void $ I.run h query
    it "should not call hRejectDisallowedImage if no avatar passed" $ do
      let query = stubQuery {I.qAvatar = Nothing}
          h =
            stubHandle
              {I.hRejectDisallowedImage = \_ -> error "Must not invoke"}
      void $ I.run h query
    it "should throw exception from hRejectDisallowedImage" $ do
      let query = stubQuery {I.qAvatar = Just stubImage}
          expectedError = "q"
          h = stubHandle {I.hRejectDisallowedImage = \_ -> error expectedError}
      I.run h query `shouldThrow` errorCall expectedError
    it
      "should not invoke hCreateUser if hRejectDisallowedImage threw an exception" $ do
      let query = stubQuery {I.qAvatar = Just stubImage}
          expectedError = "q"
          h =
            stubHandle
              { I.hRejectDisallowedImage = \_ -> error expectedError
              , I.hCreateUser = \_ -> error "Must not invoke hCreateUser"
              }
      I.run h query `shouldThrow` errorCall expectedError

stubHandle :: I.Handle IO
stubHandle =
  I.Handle
    { hCreateUser = const $ pure stubCreateUserResult
    , hGenerateToken = pure (stubToken, stubTokenHash)
    , hGetCurrentTime = stubGetCurrentTime
    , hRejectDisallowedImage = \_ -> pure ()
    }

stubToken :: Auth.SecretToken
stubToken = Auth.SecretToken ""

stubTokenHash :: Auth.SecretTokenHash
stubTokenHash = Auth.SecretTokenHash ""

stubGetCurrentTime :: IO UTCTime
stubGetCurrentTime = pure $ UTCTime (ModifiedJulianDay 6666) 0

stubCreateUserResult :: I.CreateUserResult
stubCreateUserResult =
  I.CreateUserResult {curUserId = UserId 0, curAvatarId = Nothing}

stubImage :: Image
stubImage = Image {imageContentType = "", imageData = ""}

stubQuery :: I.Query
stubQuery =
  I.Query {qFirstName = Just "John", qLastName = "Doe", qAvatar = Nothing}
