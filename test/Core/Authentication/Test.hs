module Core.Authentication.Test
  ( itShouldAuthenticateBeforeOperation
  , noCredentials
  , noOpAuthenticationHandle
  ) where

import Control.Exception
import Core.Authentication
import Core.Exception
import Core.User
import Data.IORef
import Test.Hspec

-- | Tests that the given action succeds only when the authenticated
-- user is an administrator. It passes stub credentials,
-- authentication handle, and the success continuation to the tested
-- action. The success continuation must be invoked from within the
-- action to indicate operation success.
itShouldAuthenticateBeforeOperation ::
     (Maybe Credentials -> AuthenticationHandle IO -> IO () -> IO ()) -> Spec
itShouldAuthenticateBeforeOperation test = do
  it "should succeed if authenticated successfully" $ do
    (successCont, expectationCheck) <-
      makeExpectationMustBeInvokedOnce "Continuation must be invoked"
    let h = noOpAuthenticationHandle
    test noCredentials h successCont
    expectationCheck
  it "should throw BadCredentialsException in case of bad credentials" $ do
    let successCont =
          expectationFailure
            "The action to authorize must not invoke the success continuation for incorrect credentials"
        h = AuthenticationHandle $ \_ -> throwIO $ BadCredentialsException ""
    test noCredentials h successCont `shouldThrow` isBadCredentialsException
  itShouldPassCredentialsToAuthenticationHandle Nothing test
  itShouldPassCredentialsToAuthenticationHandle (Just someTokenCredentials) test

itShouldPassCredentialsToAuthenticationHandle ::
     Maybe Credentials
  -> (Maybe Credentials -> AuthenticationHandle IO -> IO () -> IO ())
  -> Spec
itShouldPassCredentialsToAuthenticationHandle credentials test =
  it
    ("should pass credentials to the authentication handle: " ++
     show credentials) $ do
    passedCredentials <- newIORef Nothing
    let h =
          AuthenticationHandle $ \creds -> do
            modifyIORef' passedCredentials $
              maybe
                (Just creds)
                (error "Authentication is invoked more than once")
            pure AnonymousUser
    test credentials h (pure ())
    readIORef passedCredentials `shouldReturn` Just credentials

someTokenCredentials :: Credentials
someTokenCredentials =
  TokenCredentials (UserId 85265) (SecretToken "fjeskdfjgoi3h")

makeExpectationMustBeInvokedOnce :: String -> IO (IO (), IO ())
makeExpectationMustBeInvokedOnce = makeExpectationMustBeInvokedTimes 1

makeExpectationMustBeInvokedTimes :: Int -> String -> IO (IO (), IO ())
makeExpectationMustBeInvokedTimes expected name = do
  counter <- newIORef 0
  let expectation = do
        current <- readIORef counter
        if current == expected
          then failure $ succ current
          else writeIORef counter $ succ current
      assertion = do
        current <- readIORef counter
        if current /= expected
          then failure current
          else pure ()
  pure (expectation, assertion)
  where
    failure current =
      expectationFailure $
      "Expectation " ++
      show name ++
      " is invoked " ++
      show current ++ " times, while " ++ show expected ++ " expected"

noOpAuthenticationHandle :: AuthenticationHandle IO
noOpAuthenticationHandle = AuthenticationHandle $ \_ -> pure AnonymousUser

noCredentials :: Maybe Credentials
noCredentials = Nothing
