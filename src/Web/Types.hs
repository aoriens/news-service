module Web.Types
  ( EApplication
  , EMiddleware
  , Session(..)
  , SessionId
  , Wai.Application
  , Wai.Request
  , Wai.requestMethod
  , Wai.pathInfo
  , Wai.requestBodyLength
  , Wai.requestHeaders
  , Wai.queryString
  , Wai.remoteHost
  , Wai.rawPathInfo
  , Wai.defaultRequest
  , Wai.Response
  , Wai.ResponseReceived
  , Wai.responseBuilder
  , Wai.RequestBodyLength(..)
  , Wai.strictRequestBody
  , Wai.getRequestBodyChunk
  , Wai.responseToStream
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
