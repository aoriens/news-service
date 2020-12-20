{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

-- | The logger interface module. It should not define a specific
-- implementation.
module Logger
  ( Logger(..)
  , Handle(..)
  , Level(..)
  , CallSite(..)
  , debug
  , info
  , warn
  , error
  , log
  , mapMessage
  ) where

import Data.Maybe
import qualified Data.Text as T
import GHC.Stack
import Prelude hiding (error, log)

-- | The actual logger implementation is hidden here.
class Logger t m | t -> m where
  lowLevelLog :: t -> Level -> Maybe CallSite -> T.Text -> m () -- ^ Logs a message

-- | A flexible implementation of Logger - a Logger class adapter.
newtype Handle m =
  Handle
    { hLowLevelLog :: Level -> Maybe CallSite -> T.Text -> m ()
    }

instance Logger (Handle m) m where
  lowLevelLog = hLowLevelLog

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

debug, info, warn, error :: (HasCallStack, Logger t m) => t -> T.Text -> m ()
debug h = lowLevelLog h Debug (captureTopCallSite callStack)

info h = lowLevelLog h Info (captureTopCallSite callStack)

warn h = lowLevelLog h Warning (captureTopCallSite callStack)

error h = lowLevelLog h Error (captureTopCallSite callStack)

log :: (HasCallStack, Logger t m) => t -> Level -> T.Text -> m ()
log h level = lowLevelLog h level (captureTopCallSite callStack)

data CallSite =
  CallSite
    { csModule :: T.Text
    , csStartLine :: Int
    }

captureTopCallSite :: CallStack -> Maybe CallSite
captureTopCallSite = fmap makeCallSite . listToMaybe . getCallStack
  where
    makeCallSite (_, loc) =
      CallSite
        { csModule = T.pack $ srcLocModule loc
        , csStartLine = srcLocStartLine loc
        }

-- | Creates a logger handle which transforms the message and passes
-- it to another handle.
mapMessage :: (T.Text -> T.Text) -> Handle m -> Handle m
mapMessage f h =
  Handle
    { hLowLevelLog =
        \level callsite text -> hLowLevelLog h level callsite (f text)
    }
