module Core.Authentication.Test
  ( itShouldRequireAdminPermission
  , stubAuthHandleReturningAdminUser
  , noCredentials
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
itShouldRequireAdminPermission ::
     (Maybe Credentials -> AuthenticationHandle IO -> IO () -> IO ()) -> Spec
itShouldRequireAdminPermission test = do
  it "should succeed if the actor is an administrator" $ do
    (successCont, expectationCheck) <-
      makeExpectationMustBeInvokedOnce "Continuation must be invoked"
    let h = stubAuthHandleReturningAdminUser
    test noCredentials h successCont
    expectationCheck
  it "should throw NoPermissionException if the user is an identified non-admin" $ do
    let successCont =
          expectationFailure
            "The action to authorize must not invoke the success continuation for an identified non-admin user"
        h = stubAuthHandleReturningIdentifiedNonAdminUser
    test noCredentials h successCont `shouldThrow` isNoPermissionException
  it "should throw NoPermissionException if the user is anonymous" $ do
    let successCont =
          expectationFailure
            "The action to authorize must not invoke the success continuation for an anonymous user"
        h = stubAuthHandleReturningAnonymousUser
    test noCredentials h successCont `shouldThrow` isNoPermissionException
  it "should throw BadCredentialsException in case of bad credentials" $ do
    let successCont =
          expectationFailure
            "The action to authorize must not invoke the success continuation for incorrect credentials"
        h = stubAuthHandleThrowingBadCredentialsException
    test noCredentials h successCont `shouldThrow` isBadCredentialsException

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

stubAuthHandleReturningAdminUser :: AuthenticationHandle IO
stubAuthHandleReturningAdminUser =
  AuthenticationHandle $ \_ -> pure $ IdentifiedUser (UserId 276194) True

stubAuthHandleReturningIdentifiedNonAdminUser :: AuthenticationHandle IO
stubAuthHandleReturningIdentifiedNonAdminUser =
  AuthenticationHandle $ \_ -> pure $ IdentifiedUser (UserId 276194) False

stubAuthHandleReturningAnonymousUser :: AuthenticationHandle IO
stubAuthHandleReturningAnonymousUser =
  AuthenticationHandle $ \_ -> pure AnonymousUser

stubAuthHandleThrowingBadCredentialsException :: AuthenticationHandle IO
stubAuthHandleThrowingBadCredentialsException =
  AuthenticationHandle $ \_ -> throwIO $ BadCredentialsException ""

noCredentials :: Maybe Credentials
noCredentials = Nothing
