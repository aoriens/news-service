module Web.Types
  ( EApplication
  , EMiddleware
  , Session(..)
  , SessionId
  ) where

import qualified Network.Wai as Wai
import Web.Types.Internal.SessionId

type EApplication = Session -> Wai.Application

type EMiddleware = EApplication -> EApplication

-- | Shared data within single HTTP request processing.
newtype Session =
  Session
    { sessionId :: SessionId
    }
