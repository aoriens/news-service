-- | Utilities for everyday synchronous exception handling.
module Control.Exception.Sync
  ( catchS
  , catchJustS
  ) where

import Control.Exception

-- | Mimics 'Control.Exception.catch', but runs the handler outside an
-- asynchronous exception mask.
catchS :: Exception e => IO a -> (e -> IO a) -> IO a
catchS action handler = try action >>= either handler pure

-- | Mimics 'Control.Exception.catchJust', but runs the handler
-- outside an asynchronous exception mask.
catchJustS :: Exception e => (e -> Maybe z) -> IO a -> (z -> IO a) -> IO a
catchJustS extract action handler =
  tryJust extract action >>= either handler pure
