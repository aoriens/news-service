-- | The session identifier module. The module exposes API to create a
-- session id, so it's not recommended to use. Import 'Web.Session'
-- instead.
module Web.Application.Internal.SessionId
  ( SessionId(..)
  ) where

import Data.Int

newtype SessionId =
  SessionId Int64
  deriving (Eq)

instance Show SessionId where
  show (SessionId n) = show n
