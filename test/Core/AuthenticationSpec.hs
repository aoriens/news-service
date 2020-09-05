module Core.AuthenticationSpec
  ( spec
  ) where

import qualified Core.Authentication as A
import Core.Exception
import Core.User
import Data.IORef
import qualified Logger
import System.IO.Unsafe
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "authenticate" $ do
    it "should return AnonymousUser if no credentials passed" $ do
      A.authenticate stubHandle Nothing `shouldReturn` A.AnonymousUser
    it "should throw BadCredentialsException if there is no such user" $ do
      let h = stubHandle {A.hGetUserAuthData = const $ pure Nothing}
      A.authenticate h (Just stubCreds) `shouldThrow` isBadCredentialsException
    it "should throw BadCredentialsException if the secret token mismatches" $ do
      let h =
            stubHandle
              { A.hGetUserAuthData = \_ -> pure stubOKAuthData
              , A.hTokenMatchesHash = \_ _ -> False
              }
      A.authenticate h (Just stubCreds) `shouldThrow` isBadCredentialsException
    it "should throw BadCredentialsException if the secret token mismatches" $ do
      let h =
            stubHandle
              { A.hGetUserAuthData = \_ -> pure stubOKAuthData
              , A.hTokenMatchesHash = \_ _ -> False
              }
      A.authenticate h (Just stubCreds) `shouldThrow` isBadCredentialsException
    it "should return IdentifiedUser with the requested UserId on auth success" $ do
      let expectedUserId = UserId 2
          creds = A.TokenCredentials expectedUserId stubSecretToken
          h =
            stubHandle
              { A.hGetUserAuthData = \_ -> pure stubOKAuthData
              , A.hTokenMatchesHash = \_ _ -> True
              }
      A.IdentifiedUser uid _ <- A.authenticate h $ Just creds
      uid `shouldBe` expectedUserId
    it "should return IsAdmin from hGetUserAuthData on auth success" $
      property $ \expectedIsAdmin -> do
        let h =
              stubHandle
                { A.hGetUserAuthData =
                    \_ -> pure $ Just (A.SecretTokenHash "", expectedIsAdmin)
                , A.hTokenMatchesHash = \_ _ -> True
                }
        A.IdentifiedUser _ isAdmin <- A.authenticate h $ Just stubCreds
        isAdmin `shouldBe` expectedIsAdmin
    it "should pass UserId to hGetUserAuthData" $ do
      ref <- newIORef undefined
      let expectedUserId = UserId 8
          h =
            stubHandle
              { A.hGetUserAuthData =
                  \uid -> writeIORef ref uid >> pure stubOKAuthData
              , A.hTokenMatchesHash = \_ _ -> True
              }
      _ <-
        A.authenticate h $
        Just $ A.TokenCredentials expectedUserId stubSecretToken
      readIORef ref `shouldReturn` expectedUserId
    it "should pass secret token to hTokenMatchesHash" $ do
      ref <- newIORef undefined
      let expectedToken = A.SecretToken "1"
          h =
            stubHandle
              { A.hGetUserAuthData = \_ -> pure stubOKAuthData
              , A.hTokenMatchesHash =
                  \token _ ->
                    unsafePerformIO $ writeIORef ref token >> pure True
              }
      _ <- A.authenticate h $ Just $ A.TokenCredentials (UserId 1) expectedToken
      readIORef ref `shouldReturn` expectedToken
    it "should pass token hash from hGetUserAuthData to hTokenMatchesHash" $ do
      ref <- newIORef undefined
      let expectedHash = A.SecretTokenHash "1"
          h =
            stubHandle
              { A.hGetUserAuthData = \_ -> pure $ Just (expectedHash, False)
              , A.hTokenMatchesHash =
                  \_ hash -> unsafePerformIO $ writeIORef ref hash >> pure True
              }
      _ <- A.authenticate h $ Just stubCreds
      readIORef ref `shouldReturn` expectedHash

stubHandle :: A.Handle IO
stubHandle =
  A.Handle
    { hGetUserAuthData = undefined
    , hTokenMatchesHash = \_ _ -> undefined
    , hLoggerHandle = Logger.Handle {hLowLevelLog = \_ _ _ -> pure ()}
    }

stubCreds :: A.Credentials
stubCreds = A.TokenCredentials (UserId 1) stubSecretToken

stubSecretToken :: A.SecretToken
stubSecretToken = A.SecretToken ""

stubOKAuthData :: Maybe (A.SecretTokenHash, A.IsAdmin)
stubOKAuthData = Just (A.SecretTokenHash "", False)
