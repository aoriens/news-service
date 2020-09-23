module Test.AsyncExpectation
  ( shouldInvokeOnce
  , shouldPassValue
  ) where

import Control.Monad
import Data.IORef
import Test.Hspec

-- | Asserts that the tested action invokes the continuation exactly
-- once.
--
-- > it "should invoke hAuthenticate exactly once" $ do
-- >   shouldInvokeOnce "hAuthenticate invocation" $ \onSuccess -> do
-- >     let handle = Handle { hAuthenticate = \_ _ -> onSuccess }
-- >     testedCodeWithHandle handle
shouldInvokeOnce :: HasCallStack => String -> (IO () -> IO ()) -> IO ()
shouldInvokeOnce = shouldInvokeTimes 1

shouldInvokeTimes :: HasCallStack => Int -> String -> (IO () -> IO ()) -> IO ()
shouldInvokeTimes expected name test = do
  counter <- newIORef 0
  test $ onSuccess counter
  check counter
  where
    onSuccess counter = do
      current <- readIORef counter
      if current == expected
        then failure $ succ current
        else modifyIORef' counter succ
    check counter = do
      current <- readIORef counter
      when (current /= expected) $ failure current
    failure current =
      expectationFailure $
      "Expectation " ++
      show name ++
      " is invoked " ++
      show current ++ " times, while " ++ show expected ++ " expected"

-- | Asserts that the tested action passed an expected data to a
-- continuation function.
--
-- > it "should pass accepted credentials to hAuthenticate" $ do
-- >   let expectedCredentials = ("user", "password")
-- >   shouldPassValue expectedCredentials "hAuthenticate" $ \pass -> do
-- >     let handle = Handle { hAuthenticate = \creds _ -> pass creds >> pure True }
-- >     testedCodeWithHandle handle expectedCredentials
shouldPassValue ::
     (HasCallStack, Show a, Eq a)
  => a -- ^ An expected value
  -> String -- ^ A name of function that passes a value
  -> ((a -> IO ()) -> IO ()) -- ^ The tested action to be invoked with the continuation
  -> IO ()
shouldPassValue expectedValue =
  shouldPassValueSatisfying (`shouldBe` expectedValue)

shouldPassValueSatisfying ::
     (HasCallStack, Show a)
  => (a -> Expectation)
  -> String
  -> ((a -> IO ()) -> IO ())
  -> IO ()
shouldPassValueSatisfying expectation name test = do
  acc <- newIORef False
  test $ passValue acc
  check acc
  where
    passValue acc newValue = do
      isPassed <- readIORef acc
      if isPassed
        then expectationFailure $ "'" ++ name ++ "' is invoked more than once"
        else do
          writeIORef acc True
          expectation newValue
    check acc = do
      isPassed <- readIORef acc
      unless isPassed $
        expectationFailure $
        "'" ++ name ++ "' is not invoked, although it should have been"
