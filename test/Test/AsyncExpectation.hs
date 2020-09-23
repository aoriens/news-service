module Test.AsyncExpectation
  ( shouldInvokeOnce
  ) where

import Control.Monad
import Data.IORef
import Test.Hspec

-- | Asserts that the tested action invokes the continuation exactly
-- once.
--
-- > it "should invoke hAuthenticate exactly once" $ do
-- >   assertIsInvokedOnce "hAuthenticate invocation" $ \onSuccess -> do
-- >     handle = Handle { hAuthenticate = \_ _ -> onSuccess }
-- >     testedCode handle
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
