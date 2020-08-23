{-# LANGUAGE RecordWildCards #-}

module Core.Interactor.CreateUserSpec
  ( spec
  ) where

import Control.Monad
import qualified Core.Interactor.CreateUser as I
import qualified Data.HashSet as HS
import Data.IORef
import Data.Text (Text)
import Data.Time
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
      (I.userFirstName user, I.userLastName user) `shouldBe`
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
      I.userCreatedAt user `shouldBe` expectedTime
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
    it "should return UserId from hCreateUser" $ do
      let expectedUserId = I.UserId 1
          h =
            stubHandle
              { I.hCreateUser =
                  const $
                  pure stubCreateUserResult {I.curUserId = expectedUserId}
              }
      (user, _) <- I.run h stubQuery
      I.userId user `shouldBe` expectedUserId
    it "should return curAvatarId from hCreateUser" $ do
      let expectedImageId = Just $ I.ImageId 1
          h =
            stubHandle
              { I.hCreateUser =
                  const $
                  pure stubCreateUserResult {I.curAvatarId = expectedImageId}
              }
      (user, _) <- I.run h stubQuery
      I.userAvatarId user `shouldBe` expectedImageId
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
      I.userIsAdmin user `shouldBe` False
      readIORef ref `shouldReturn` False
    it
      "should throw QueryException if avatar content type is not in the allowed list" $ do
      let disallowedContentType = "image/jpeg"
          allowedContentTypes = ["image/tiff"]
          query =
            stubQuery
              { I.qAvatar =
                  Just
                    I.Image
                      {imageContentType = disallowedContentType, imageData = ""}
              }
          h =
            stubHandle
              {I.hAllowedImageContentTypes = HS.fromList allowedContentTypes}
      I.run h query `shouldThrow` \I.QueryException {} -> True
    it
      "should not throw QueryException if avatar content type is in the allowed list" $ do
      let allowedContentType = "image/tiff"
          query =
            stubQuery
              { I.qAvatar =
                  Just
                    I.Image
                      {imageContentType = allowedContentType, imageData = ""}
              }
          h =
            stubHandle
              {I.hAllowedImageContentTypes = HS.singleton allowedContentType}
      void $ I.run h query -- should not throw

stubHandle :: I.Handle IO
stubHandle =
  I.Handle
    { hCreateUser = const $ pure stubCreateUserResult
    , hGenerateToken = pure stubTokenInfo
    , hGetCurrentTime = stubGetCurrentTime
    , hAllowedImageContentTypes = HS.singleton defaultAllowedImageContentType
    }

defaultAllowedImageContentType :: Text
defaultAllowedImageContentType = "image/png"

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
    , qAvatar =
        Just
          I.Image
            {imageContentType = defaultAllowedImageContentType, imageData = ""}
    }
