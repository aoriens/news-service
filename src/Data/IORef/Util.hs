module Data.IORef.Util
  ( updateIORef'
  ) where

import Data.IORef

updateIORef' :: IORef a -> (a -> (a, r)) -> IO r
updateIORef' ref f = do
  old <- readIORef ref
  let (new, r) = f old
  writeIORef ref $! new
  pure r
