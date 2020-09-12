-- | Utilities for everyday synchronous exception handling.
module Control.Exception.Sync
  ( catchS
  , catchJustS
  , catchJustS'
  ) where

import Control.Exception
import Control.Monad

-- | Mimics 'Control.Exception.catch', but runs the handler outside an
-- asynchronous exception mask.
catchS :: Exception e => IO a -> (e -> IO a) -> IO a
catchS action handler = try action >>= either handler pure

-- | Mimics 'Control.Exception.catchJust', but runs the handler
-- outside an asynchronous exception mask.
catchJustS :: Exception e => (e -> Maybe z) -> IO a -> (z -> IO a) -> IO a
catchJustS extract action handler =
  tryJust extract action >>= either handler pure

-- | A simplified version of 'catchJustS'. It mimics
-- 'Control.Exception.catchJust', but accepts a predicate to filter
-- exceptions to catch, does not pass the exception to the handler,
-- and runs the handler outside an asynchronous exception mask.
catchJustS' :: Exception e => (e -> Bool) -> IO a -> IO a -> IO a
catchJustS' testException action handler =
  tryJust (guard . testException) action >>= either (const handler) pure
