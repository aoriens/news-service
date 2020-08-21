{-# LANGUAGE RecordWildCards #-}

module Core.Interactor.CreateUserSpec
  ( spec
  ) where

import Control.Monad
import qualified Core.Interactor.CreateUser as I
import Data.IORef
import Data.Time
import Test.Hspec

spec :: Spec
spec = do
  describe "run" $ do
    it "should pass query fields to hCreateUser" $ do
      ref <-
        newIORef (error "Must have written here the first and the last name")
      let h =
            stubHandle
              { I.hCreateUser =
                  \I.CreateUserCommand {..} -> do
                    writeIORef ref (cuFirstName, cuLastName, cuAvatar)
                    pure stubCreateUserResult
              }
          query = stubQuery
      void $ I.run h query
      readIORef ref `shouldReturn`
        (I.qFirstName query, I.qLastName query, I.qAvatar query)
    it "should pass time from hGetCurrentTime hCreateUser" $ do
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
      void $ I.run h stubQuery
      readIORef ref `shouldReturn` expectedTime
    it "should pass the hash from hGenerateToken to hCreateUser" $ do
      ref <- newIORef (error "Must have written a hash here")
      let expectedHash = "1"
          h =
            stubHandle
              { I.hCreateUser =
                  \I.CreateUserCommand {..} -> do
                    writeIORef ref cuTokenHash
                    pure stubCreateUserResult
              , I.hGenerateToken = pure stubTokenInfo {I.stiHash = expectedHash}
              }
      void $ I.run h stubQuery
      readIORef ref `shouldReturn` expectedHash
    it "should return the token from hGenerateToken" $ do
      let expectedToken = I.SecretToken "1"
          h =
            stubHandle
              { I.hGenerateToken =
                  pure stubTokenInfo {I.stiToken = expectedToken}
              }
      (_, token) <- I.run h stubQuery
      token `shouldBe` expectedToken

stubHandle :: I.Handle IO
stubHandle =
  I.Handle
    { hCreateUser = const $ pure stubCreateUserResult
    , hGenerateToken = pure stubTokenInfo
    , hGetCurrentTime = stubGetCurrentTime
    }

stubTokenInfo :: I.SecretTokenInfo
stubTokenInfo = I.SecretTokenInfo {stiToken = I.SecretToken "", stiHash = ""}

stubGetCurrentTime :: IO UTCTime
stubGetCurrentTime = pure $ UTCTime (ModifiedJulianDay 6666) 0

stubCreateUserResult :: I.CreateUserResult
stubCreateUserResult =
  I.CreateUserResult {curUserId = I.UserId 0, curAvatarId = Nothing}

stubQuery :: I.Query
stubQuery =
  I.Query
    { qFirstName = Just "John"
    , qLastName = "Doe"
    , qAvatar = Just I.Image {imageContentType = "image/jpeg", imageData = ""}
    }
