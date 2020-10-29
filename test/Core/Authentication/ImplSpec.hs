module Core.Authentication.ImplSpec
  ( spec
  ) where

import qualified Core.Authentication as A
import qualified Core.Authentication.Impl as Impl
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
      let h = Impl.new stubHandle
      A.authenticate h Nothing `shouldReturn` A.AnonymousUser
    it "should throw BadCredentialsException if there is no such user" $ do
      let h = Impl.new stubHandle {Impl.hGetUserAuthData = const $ pure Nothing}
      A.authenticate h (Just stubCreds) `shouldThrow` isBadCredentialsException
    it "should throw BadCredentialsException if the secret token mismatches" $ do
      let h =
            Impl.new
              stubHandle
                { Impl.hGetUserAuthData = \_ -> pure stubOKAuthData
                , Impl.hTokenMatchesHash = \_ _ -> False
                }
      A.authenticate h (Just stubCreds) `shouldThrow` isBadCredentialsException
    it "should throw BadCredentialsException if the secret token mismatches" $ do
      let h =
            Impl.new
              stubHandle
                { Impl.hGetUserAuthData = \_ -> pure stubOKAuthData
                , Impl.hTokenMatchesHash = \_ _ -> False
                }
      A.authenticate h (Just stubCreds) `shouldThrow` isBadCredentialsException
    it "should return IdentifiedUser with the requested UserId on auth success" $ do
      let expectedUserId = UserId 2
          creds = A.TokenCredentials expectedUserId stubSecretToken
          h =
            Impl.new
              stubHandle
                { Impl.hGetUserAuthData = \_ -> pure stubOKAuthData
                , Impl.hTokenMatchesHash = \_ _ -> True
                }
      A.IdentifiedUser uid _ _ <- A.authenticate h $ Just creds
      uid `shouldBe` expectedUserId
    it "should return IsAdmin from hGetUserAuthData on auth success" $
      property $ \expectedIsAdmin -> do
        let h =
              Impl.new
                stubHandle
                  { Impl.hGetUserAuthData =
                      \_ ->
                        pure $
                        Just
                          stubAuthData {Impl.authDataIsAdmin = expectedIsAdmin}
                  , Impl.hTokenMatchesHash = \_ _ -> True
                  }
        A.IdentifiedUser _ isAdmin _ <- A.authenticate h $ Just stubCreds
        isAdmin `shouldBe` expectedIsAdmin
    it "should pass UserId to hGetUserAuthData" $ do
      ref <- newIORef undefined
      let expectedUserId = UserId 8
          h =
            Impl.new
              stubHandle
                { Impl.hGetUserAuthData =
                    \uid -> writeIORef ref uid >> pure (Just stubAuthData)
                , Impl.hTokenMatchesHash = \_ _ -> True
                }
      _ <-
        A.authenticate h $
        Just $ A.TokenCredentials expectedUserId stubSecretToken
      readIORef ref `shouldReturn` expectedUserId
    it "should pass secret token to hTokenMatchesHash" $ do
      ref <- newIORef undefined
      let expectedToken = A.SecretToken "1"
          h =
            Impl.new
              stubHandle
                { Impl.hGetUserAuthData = \_ -> pure stubOKAuthData
                , Impl.hTokenMatchesHash =
                    \token _ ->
                      unsafePerformIO $ writeIORef ref token >> pure True
                }
      _ <- A.authenticate h $ Just $ A.TokenCredentials (UserId 1) expectedToken
      readIORef ref `shouldReturn` expectedToken
    it "should pass token hash from hGetUserAuthData to hTokenMatchesHash" $ do
      ref <- newIORef undefined
      let expectedHash = A.SecretTokenHash "1"
          h =
            Impl.new
              stubHandle
                { Impl.hGetUserAuthData =
                    \_ ->
                      pure $
                      Just
                        stubAuthData
                          {Impl.authDataSecretTokenHash = expectedHash}
                , Impl.hTokenMatchesHash =
                    \_ hash ->
                      unsafePerformIO $ writeIORef ref hash >> pure True
                }
      _ <- A.authenticate h $ Just stubCreds
      readIORef ref `shouldReturn` expectedHash

stubHandle :: Impl.Handle IO
stubHandle =
  Impl.Handle
    { hGetUserAuthData = undefined
    , hTokenMatchesHash = \_ _ -> undefined
    , hLoggerHandle = Logger.Handle {hLowLevelLog = \_ _ _ -> pure ()}
    }

stubCreds :: A.Credentials
stubCreds = A.TokenCredentials (UserId 1) stubSecretToken

stubSecretToken :: A.SecretToken
stubSecretToken = A.SecretToken ""

stubAuthData :: Impl.UserAuthData
stubAuthData =
  Impl.UserAuthData
    { authDataSecretTokenHash = A.SecretTokenHash ""
    , authDataIsAdmin = False
    , authDataAuthors = []
    }

stubOKAuthData :: Maybe Impl.UserAuthData
stubOKAuthData = Just stubAuthData
